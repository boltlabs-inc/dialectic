use dashmap::DashMap;
use dialectic::{
    backend::{self, By},
    prelude::*,
};
use std::{
    future::Future,
    hash::Hash,
    marker::PhantomData,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Weak,
    },
    time::Duration,
};
use tokio::{pin, select, sync::mpsc, time::Instant};

pub enum ResumeError<Key, Err> {
    HandshakeIncomplete,
    HandshakeError(Err),
    NoSuchKey(Key),
    KeyAlreadyExists(Key),
    ResumeIncomplete,
}

pub enum ConnectKind {
    New,
    Existing,
}

type Handshake<H, Key, E, Tx, Rx> =
    dyn Fn(Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<(ConnectKind, Key), E>>>>;

struct Waiting<Tx, Rx> {
    send_tx: mpsc::Sender<Tx>,
    send_rx: mpsc::Sender<Rx>,
    half_dropped: AtomicBool,
}

type Managed<Key, Tx, Rx> = DashMap<Key, Waiting<Tx, Rx>>;

pub enum ResumeStrategy {
    RetryAfter(Duration),
    RetryAfterReconnect,
    Fail,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Acceptor<H, S, Key, E, Tx, Rx>
where
    H: Session,
    S: Session,
{
    handshake: Box<Handshake<H, Key, E, Tx, Rx>>,
    managed: Arc<Managed<Key, Tx, Rx>>,
    recover_tx: Arc<dyn Fn(usize, &Tx::Error) -> ResumeStrategy + Sync + Send>,
    recover_rx: Arc<dyn Fn(usize, &Rx::Error) -> ResumeStrategy + Sync + Send>,
    timeout: Option<Duration>,
    buffer_size: usize,
    session: PhantomData<fn() -> S>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, H, Err, S, Tx, Rx> Acceptor<H, S, Key, Err, Tx, Rx>
where
    Key: Clone + Eq + Hash + Send + Sync + 'static,
    H: Session,
    S: Session,
{
    pub fn new<Fut: Future<Output = Result<(ConnectKind, Key), Err>> + 'static>(
        handshake: impl Fn(Chan<H, Tx, Rx>) -> Fut + 'static,
        recover_tx: impl Fn(usize, &Tx::Error) -> ResumeStrategy + Sync + Send + 'static,
        recover_rx: impl Fn(usize, &Rx::Error) -> ResumeStrategy + Sync + Send + 'static,
        timeout: Option<Duration>,
        buffer_size: usize,
    ) -> Self {
        Self {
            handshake: Box::new(move |chan| Box::pin(handshake(chan))),
            managed: Arc::new(DashMap::new()),
            recover_tx: Arc::new(recover_tx),
            recover_rx: Arc::new(recover_rx),
            timeout,
            buffer_size,
            session: PhantomData,
        }
    }

    pub async fn accept(
        &self,
        tx: Tx,
        rx: Rx,
    ) -> Result<Option<Chan<S, Sender<Key, Tx, Rx>, Receiver<Key, Tx, Rx>>>, ResumeError<Key, Err>>
    {
        let (result, ends) = H::over(tx, rx, |chan| (self.handshake)(chan)).await;
        let (connect_kind, key) = match result {
            Ok(key) => key,
            Err(error) => return Err(ResumeError::HandshakeError(error)),
        };
        let (tx, rx) = match ends {
            Ok(ends) => ends,
            Err(_) => return Err(ResumeError::HandshakeIncomplete),
        };
        match (connect_kind, self.managed.get(&key)) {
            (ConnectKind::Existing, None) => {
                // If the connection was supposed to be for an existing key but no key
                // existed yet, return an error instead of generating a new channel.
                Err(ResumeError::NoSuchKey(key))
            }
            (ConnectKind::New, Some(_)) => {
                // If the connection was supposed to be for a new key, but the key existed,
                // return an error instead of resuming on that key.
                Err(ResumeError::KeyAlreadyExists(key))
            }
            (ConnectKind::Existing, Some(map_ref)) => {
                // If there was already an entry by this key, there's a session, potentially
                // paused, by this key. If the handshake was properly implemented, we're
                // only receiving this tx/rx pair because the other end tried to reconnect,
                // which means that the connection broke in one direction or another, so
                // either we are currently paused, or we're about to discover the issue and
                // pause. Sending the tx/rx will un-pause us and allow us to resume on those
                // fresh channels.
                let send_tx = &map_ref.send_tx;
                let send_rx = &map_ref.send_rx;

                use mpsc::error::TrySendError::Full;
                match (send_tx.try_send(tx), send_rx.try_send(rx)) {
                    // If either side bounced on send due to the channel being full, return an error
                    (Err(Full(_)), _) | (_, Err(Full(_))) => Err(ResumeError::ResumeIncomplete),
                    // Otherwise, indicate success (this is inclusive of when a receiver is closed)
                    _ => Ok(None),
                }
            }
            (ConnectKind::New, None) => {
                // If there was no entry by this key, then this represents a fresh session,
                // and we should return a channel for the caller to do with what they like
                // (usually, run a session in a separate task).
                let (send_tx, next_tx) = mpsc::channel(self.buffer_size);
                let (send_rx, next_rx) = mpsc::channel(self.buffer_size);
                self.managed.insert(
                    key.clone(),
                    Waiting {
                        send_tx,
                        send_rx,
                        half_dropped: false.into(),
                    },
                );
                let tx = Sender {
                    key: key.clone(),
                    tx,
                    next_tx,
                    recover_tx: self.recover_tx.clone(),
                    managed: Arc::downgrade(&self.managed),
                    timeout: self.timeout,
                };
                let rx = Receiver {
                    key,
                    rx,
                    next_rx,
                    recover_rx: self.recover_rx.clone(),
                    managed: Arc::downgrade(&self.managed),
                    timeout: self.timeout,
                };
                Ok(Some(S::wrap(tx, rx)))
            }
        }
    }
}

// Retry a channel operation in a loop until the error recovery routine indicates it's time to stop.
//
// This is a macro because as far as I can tell it's not possible to express as a HRTB the bounds
// necessary to quantify over the action to be performed, in the ref case.
macro_rules! retry_loop {
    ($end:expr, $next:expr, $timeout:expr, $recover:expr, $action:expr $(,)?) => {{
        let mut count = 0;
        let mut deadline: Option<Instant> = None;
        loop {
            match $action.await {
                Ok(t) => break Ok(t),
                Err(error) => {
                    // Create a timeout future that will expire `timeout` duration after the first
                    // error we encountered
                    let timeout = $timeout;
                    let timeout = async move {
                        if let Some(duration) = timeout {
                            if count == 0 {
                                deadline = Some(Instant::now() + duration);
                            }
                            // If the timeout was specified, sleep until now plus that duration
                            tokio::time::sleep_until(deadline.unwrap()).await
                        } else {
                            // If no timeout was specified, sleep forever
                            std::future::pending().await
                        }
                    };

                    pin!(timeout);

                    match $recover(count, &error) {
                        ResumeStrategy::RetryAfter(after) => {
                            // If the retry interval happens before the deadline, retry
                            let sleep = tokio::time::sleep(after);
                            pin!(sleep);
                            select! {
                                () = sleep => {},
                                () = timeout => break Err(error),
                            }
                        }
                        ResumeStrategy::RetryAfterReconnect => {
                            // If the connection is reconnected before the deadline, reconnect
                            // and retry
                            // TODO: drop the connection immediately here!
                            let recv = $next.recv();
                            pin!(recv);
                            select! {
                                result = recv => match result {
                                    Some(new_end) => $end = new_end,
                                    None => break Err(error),
                                },
                                () = timeout => break Err(error),
                            }
                        }
                        ResumeStrategy::Fail => {
                            break Err(error);
                        }
                    }

                    // Increase the count, so next iteration we know how to recover differently
                    count += 1;
                }
            }
        }
    }};
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Sender<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    key: Key,
    tx: Tx,
    next_tx: mpsc::Receiver<Tx>,
    recover_tx: Arc<dyn Fn(usize, &Tx::Error) -> ResumeStrategy + Sync + Send>,
    managed: Weak<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Tx, Rx> Drop for Sender<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    fn drop(&mut self) {
        // Remove the entire entry at this key if the other end has already dropped
        self.managed.upgrade().map(|managed| {
            managed.remove_if(&self.key, |_, waiting| {
                waiting.half_dropped.swap(true, Ordering::SeqCst)
            })
        });
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Tx, Rx> Transmitter for Sender<Key, Tx, Rx>
where
    Key: Sync + Send + Eq + Hash,
    Tx::Error: Send + Sync,
{
    type Error = Tx::Error;

    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move {
            retry_loop!(
                self.tx,
                self.next_tx,
                self.timeout,
                self.recover_tx,
                self.tx.send_choice(choice),
            )
        })
    }
}

#[Transmitter(Tx for ref T)]
#[Receiver(Rx)]
impl<T, Key, Tx, Rx> Transmit<T, Val> for Sender<Key, Tx, Rx>
where
    T: Send + Sync + 'static,
    Key: Sync + Send + Eq + Hash,
    Tx::Error: Send + Sync,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: By<'a, Val>,
        'a: 'async_lifetime,
    {
        let message: T = call_by::to_val(message);
        Box::pin(async move {
            retry_loop!(
                self.tx,
                self.next_tx,
                self.timeout,
                self.recover_tx,
                self.tx.send(&message),
            )
        })
    }
}

#[Transmitter(Tx for ref T)]
#[Receiver(Rx)]
impl<T, Key, Tx, Rx> Transmit<T, Ref> for Sender<Key, Tx, Rx>
where
    for<'a> <T as By<'a, Ref>>::Type: Send,
    T: Sync + 'static,
    Key: Sync + Send + Eq + Hash,
    Tx::Error: Send + Sync,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Ref>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: By<'a, Ref>,
        'a: 'async_lifetime,
    {
        let message: &'a T = call_by::to_ref::<T>(message);
        Box::pin(async move {
            retry_loop!(
                self.tx,
                self.next_tx,
                self.timeout,
                self.recover_tx,
                self.tx.send(call_by::from_ref::<T>(message)),
            )
        })
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Receiver<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    key: Key,
    rx: Rx,
    next_rx: mpsc::Receiver<Rx>,
    recover_rx: Arc<dyn Fn(usize, &Rx::Error) -> ResumeStrategy + Sync + Send>,
    managed: Weak<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Tx, Rx> Drop for Receiver<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    fn drop(&mut self) {
        // Remove the entire entry at this key if the other end has already dropped
        self.managed.upgrade().map(|managed| {
            managed.remove_if(&self.key, |_, waiting| {
                waiting.half_dropped.swap(true, Ordering::SeqCst)
            })
        });
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Tx, Rx> backend::Receiver for Receiver<Key, Tx, Rx>
where
    Key: Send + Sync + Eq + Hash,
    Rx::Error: Send + Sync,
{
    type Error = Rx::Error;

    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        Box::pin(async move {
            retry_loop!(
                self.rx,
                self.next_rx,
                self.timeout,
                self.recover_rx,
                self.rx.recv_choice(),
            )
        })
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx for T)]
impl<T, Key, Tx, Rx> Receive<T> for Receiver<Key, Tx, Rx>
where
    T: Send + 'static,
    Key: Send + Sync + Eq + Hash,
    Rx::Error: Send + Sync,
{
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move {
            retry_loop!(
                self.rx,
                self.next_rx,
                self.timeout,
                self.recover_rx,
                self.rx.recv(),
            )
        })
    }
}
