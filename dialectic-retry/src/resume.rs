use async_trait::async_trait;
use dashmap::DashMap;
use dialectic::{
    backend::{self, By},
    prelude::*,
    IncompleteHalf, SessionIncomplete,
};
use futures::{ready, stream::FuturesUnordered};
use pin_project::pin_project;
use std::{
    future::Future,
    hash::Hash,
    marker::PhantomData,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Weak,
    },
    task::{Context, Poll},
    time::Duration,
};
use tokio::{
    pin, select,
    sync::mpsc::{self, error::TrySendError},
    time::{Instant, Sleep},
};

#[async_trait]
pub trait Resume<Key, Tx, Rx>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    type Session: Session;
    type Error;

    async fn handshake(
        &self,
        chan: Chan<Self::Session, Tx, Rx>,
    ) -> Result<(ConnectKind, Key), Self::Error>;
}

pub enum ConnectKind {
    New,
    Existing,
}

pub enum ErrorStrategy {
    Retry(Duration),
    RetryAfterReconnect,
    Fail,
}

type Managed<Key, Tx, Rx> = DashMap<Key, (mpsc::Sender<Tx>, mpsc::Sender<Rx>)>;

type RecoverTx<Tx> = fn(&<Tx as backend::Transmitter>::Error, usize) -> ErrorStrategy;

type RecoverRx<Rx> = fn(&<Rx as backend::Receiver>::Error, usize) -> ErrorStrategy;

#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Clone)]
pub struct Resumer<R, Key, S, Tx, Rx> {
    resume: R,
    queue_size: usize,
    managed: Arc<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
    recover_tx: RecoverTx<Tx>,
    recover_rx: RecoverRx<Rx>,
    session: PhantomData<S>,
}

pub enum ResumeError<R, Key, Tx, Rx>
where
    R: Resume<Key, Tx, Rx>,
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    HandshakeIncomplete(SessionIncomplete<Tx, Rx>),
    HandshakeError {
        error: R::Error,
        tx: Result<Tx, IncompleteHalf<Tx>>,
        rx: Result<Rx, IncompleteHalf<Rx>>,
    },
    NoSuchKey {
        key: Key,
        tx: Tx,
        rx: Rx,
    },
    KeyAlreadyExists {
        key: Key,
        tx: Tx,
        rx: Rx,
    },
    ResumeIncomplete(ResumeIncomplete<Tx, Rx>),
}

pub enum ResumeIncomplete<Tx, Rx> {
    BothHalves {
        tx_error: TrySendError<Tx>,
        rx_error: TrySendError<Rx>,
    },
    TxHalf {
        tx_error: TrySendError<Tx>,
    },
    RxHalf {
        rx_error: TrySendError<Rx>,
    },
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<R, Key, S, Tx, Rx> Resumer<R, Key, S, Tx, Rx>
where
    R: Resume<Key, Tx, Rx>,
    Key: Clone + Eq + Hash + Send + Sync + 'static,
    S: Session,
{
    pub async fn resume(
        &self,
        tx: Tx,
        rx: Rx,
    ) -> Result<
        Option<Chan<S, Sender<Key, Tx, Rx>, Receiver<Key, Tx, Rx>>>,
        ResumeError<R, Key, Tx, Rx>,
    > {
        let (result, ends) = <R::Session>::over(tx, rx, |chan| self.resume.handshake(chan)).await;
        let (connect_kind, key) = match result {
            Ok(key) => key,
            Err(error) => {
                let (tx, rx) = ends
                    .map(|(tx, rx)| (Ok(tx), Ok(rx)))
                    .unwrap_or_else(SessionIncomplete::into_halves);
                return Err(ResumeError::HandshakeError { error, tx, rx });
            }
        };
        match ends {
            Err(incomplete) => Err(ResumeError::HandshakeIncomplete(incomplete)),
            Ok((tx, rx)) => {
                match (connect_kind, self.managed.get(&key)) {
                    (ConnectKind::Existing, None) => {
                        // If the connection was supposed to be for an existing key but no key
                        // existed yet, return an error instead of generating a new channel.
                        Err(ResumeError::NoSuchKey { key, tx, rx })
                    }
                    (ConnectKind::New, Some(_)) => {
                        // If the connection was supposed to be for a new key, but the key existed,
                        // return an error instead of resuming on that key.
                        Err(ResumeError::KeyAlreadyExists { key, tx, rx })
                    }
                    (ConnectKind::Existing, Some(map_ref)) => {
                        // If there was already an entry by this key, there's a session, potentially
                        // paused, by this key. If the handshake was properly implemented, we're
                        // only receiving this tx/rx pair because the other end tried to reconnect,
                        // which means that the connection broke in one direction or another, so
                        // either we are currently paused, or we're about to discover the issue and
                        // pause. Sending the tx/rx will un-pause us and allow us to resume on those
                        // fresh channels.
                        let send_tx = &map_ref.0;
                        let send_rx = &map_ref.1;
                        match (send_tx.try_send(tx), send_rx.try_send(rx)) {
                            (Ok(()), Ok(())) => Ok(None),
                            (Err(tx_error), Err(rx_error)) => Err(ResumeError::ResumeIncomplete(
                                ResumeIncomplete::BothHalves { tx_error, rx_error },
                            )),
                            (Err(tx_error), Ok(())) => {
                                Err(ResumeError::ResumeIncomplete(ResumeIncomplete::TxHalf {
                                    tx_error,
                                }))
                            }
                            (Ok(()), Err(rx_error)) => {
                                Err(ResumeError::ResumeIncomplete(ResumeIncomplete::RxHalf {
                                    rx_error,
                                }))
                            }
                        }
                    }
                    (ConnectKind::New, None) => {
                        // If there was no entry by this key, then this represents a fresh session,
                        // and we should return a channel for the caller to do with what they like
                        // (usually, run a session in a separate task).
                        let (send_next_tx, next_tx) = mpsc::channel(self.queue_size);
                        let (send_next_rx, next_rx) = mpsc::channel(self.queue_size);
                        self.managed
                            .insert(key.clone(), (send_next_tx, send_next_rx));
                        let tx = Sender {
                            key: key.clone(),
                            tx,
                            next_tx,
                            recover_tx: self.recover_tx,
                            managed: Arc::downgrade(&self.managed),
                            timeout: self.timeout,
                        };
                        let rx = Receiver {
                            key,
                            rx,
                            next_rx,
                            recover_rx: self.recover_rx,
                            managed: Arc::downgrade(&self.managed),
                            timeout: self.timeout,
                        };
                        Ok(Some(S::wrap(tx, rx)))
                    }
                }
            }
        }
    }
}

async fn retry_loop<'a, C, Fut, T, E>(
    end: &'a mut C,
    next: &mut mpsc::Receiver<C>,
    timeout: Option<Duration>,
    recover: impl Fn(&E, usize) -> ErrorStrategy,
    action: impl Fn(&'a mut C) -> Fut,
) -> Result<T, E>
where
    Fut: Future<Output = Result<T, E>> + 'a,
{
    let mut count = 0;
    let mut deadline: Option<Instant> = None;
    loop {
        let result = action(end).await;
        match result {
            Ok(t) => break Ok(t),
            Err(error) => {
                // Compute the deadline for any retries as the timeout duration after the time
                // of the first error (this never changes after this point)
                if let Some(duration) = timeout {
                    if count == 0 {
                        deadline = Some(Instant::now() + duration);
                    }
                }

                // Create a timeout at the deadline
                let timeout = tokio::time::sleep_until(deadline.unwrap());
                pin!(timeout);

                match recover(&error, count) {
                    ErrorStrategy::Retry(after) => {
                        // If the retry interval happens before the deadline, retry
                        let sleep = tokio::time::sleep(after);
                        pin!(sleep);
                        select! {
                            () = sleep => {},
                            () = timeout => break Err(error),
                        }
                    }
                    ErrorStrategy::RetryAfterReconnect => {
                        // If the connection is reconnected before the deadline, reconnect and retry
                        let recv = next.recv();
                        pin!(recv);
                        select! {
                            result = recv => match result {
                                Some(new_end) => *end = new_end,
                                None => break Err(error),
                            },
                            () = timeout => break Err(error),
                        }
                    }
                    ErrorStrategy::Fail => {
                        break Err(error);
                    }
                }

                // Increase the count, so next iteration we know how to recover differently
                count += 1;
            }
        }
    }
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
    recover_tx: RecoverTx<Tx>,
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
        self.managed
            .upgrade()
            .map(|managed| managed.remove_if(&self.key, |_, (_, send_rx)| send_rx.is_closed()));
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
        Box::pin(retry_loop(
            &mut self.tx,
            &mut self.next_tx,
            self.timeout,
            self.recover_tx,
            |tx| tx.send_choice(choice),
        ))
    }
}

#[Transmitter(Tx for ref T)]
#[Receiver(Rx)]
impl<T, Key, Tx, Rx> Transmit<T, Val> for Sender<Key, Tx, Rx>
where
    T: Sync + Send + 'static,
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
        let message = call_by::to_val(message);
        Box::pin(async move {
            let mut count = 0;
            loop {
                if let Err(error) = self.tx.send(&message).await {
                    match (self.recover_tx)(&error, count) {
                        ErrorStrategy::Retry(after) => {
                            tokio::time::sleep(after).await;
                        }
                        ErrorStrategy::RetryAfterReconnect => {
                            if let Some(tx) = self.next_tx.recv().await {
                                self.tx = tx;
                            } else {
                                break Err(error);
                            }
                        }
                        ErrorStrategy::Fail => {
                            break Err(error);
                        }
                    }
                    count += 1;
                } else {
                    break Ok(());
                }
            }
        })
    }
}

#[Transmitter(Tx for ref T)]
#[Receiver(Rx)]
impl<T, Key, Tx, Rx> Transmit<T, Ref> for Sender<Key, Tx, Rx>
where
    for<'a> <T as By<'a, Ref>>::Type: Send,
    T: Sync + Send + 'static,
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
            let mut count = 0;
            loop {
                if let Err(error) = self.tx.send(call_by::from_ref::<T>(message)).await {
                    match (self.recover_tx)(&error, count) {
                        ErrorStrategy::Retry(after) => {
                            tokio::time::sleep(after).await;
                        }
                        ErrorStrategy::RetryAfterReconnect => {
                            if let Some(tx) = self.next_tx.recv().await {
                                self.tx = tx;
                            } else {
                                break Err(error);
                            }
                        }
                        ErrorStrategy::Fail => {
                            break Err(error);
                        }
                    }
                    count += 1;
                } else {
                    break Ok(());
                }
            }
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
    recover_rx: RecoverRx<Rx>,
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
        self.managed
            .upgrade()
            .map(|managed| managed.remove_if(&self.key, |_, (send_tx, _)| send_tx.is_closed()));
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
            let mut count = 0;
            loop {
                match self.rx.recv_choice().await {
                    Err(error) => {
                        match (self.recover_rx)(&error, count) {
                            ErrorStrategy::Retry(after) => {
                                tokio::time::sleep(after).await;
                            }
                            ErrorStrategy::RetryAfterReconnect => {
                                if let Some(rx) = self.next_rx.recv().await {
                                    self.rx = rx;
                                } else {
                                    break Err(error);
                                }
                            }
                            ErrorStrategy::Fail => {
                                break Err(error);
                            }
                        }
                        count += 1;
                    }
                    Ok(choice) => break Ok(choice),
                }
            }
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
            let mut count = 0;
            loop {
                match self.rx.recv().await {
                    Err(error) => {
                        match (self.recover_rx)(&error, count) {
                            ErrorStrategy::Retry(after) => {
                                tokio::time::sleep(after).await;
                            }
                            ErrorStrategy::RetryAfterReconnect => {
                                if let Some(rx) = self.next_rx.recv().await {
                                    self.rx = rx;
                                } else {
                                    break Err(error);
                                }
                            }
                            ErrorStrategy::Fail => {
                                break Err(error);
                            }
                        }
                        count += 1;
                    }
                    Ok(t) => break Ok(t),
                }
            }
        })
    }
}
