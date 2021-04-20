use dialectic::{
    backend::{self, By},
    prelude::*,
};
use std::{
    collections::VecDeque,
    future::Future,
    pin::Pin,
    sync::{Arc, Weak},
    time::Duration,
};
use tokio::{
    sync::{Mutex, OwnedMutexGuard},
    time::Instant,
};

/// A description of what to do when an error happens that we might want to recover from.
pub enum RetryStrategy {
    /// Pause for this amount of time, then retry without discarding the current connection.
    RetryAfter(Duration),
    /// Discard the current connection, then retry after this amount of time.
    ReconnectAfter(Duration),
    /// Immediately fail, propagating the error.
    Fail,
}

/// A description of what to do when an error happens during reconnection.
pub enum ReconnectStrategy {
    /// Pause for this amount of time, then attempt to reconnect again.
    ReconnectAfter(Duration),
    /// Immediately fail, propagating the error.
    Fail,
}

pub enum RetryError<Err, ConnectError, HandshakeError> {
    OriginalError(Err),
    ConnectError(ConnectError),
    HandshakeError(HandshakeError),
    HandshakeIncomplete,
}

type Handshake<H, Key, Err, Tx, Rx> = dyn Fn(Option<&Key>, Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<Key, Err>> + Send>>
    + Send
    + Sync;

type Connect<Err, Tx, Rx> =
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<(Tx, Rx), Err>> + Send>> + Send + Sync;

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub async fn channel<H, S, Key, ConnectErr, HandshakeErr, ConnectFut, HandshakeFut, Tx, Rx>(
    connect: impl Fn() -> ConnectFut + Sync + Send + 'static,
    handshake: impl Fn(Option<&Key>, Chan<H, Tx, Rx>) -> HandshakeFut + Sync + Send + 'static,
    recover_connect: impl Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send + 'static,
    recover_handshake: impl Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send + 'static,
    recover_tx: impl Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send + 'static,
    recover_rx: impl Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send + 'static,
    timeout: Option<Duration>,
) -> Chan<
    S,
    Sender<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
    Receiver<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
>
where
    S: Session,
    H: Session,
    Key: Sync + Send + Clone + 'static,
    ConnectFut: Future<Output = Result<(Tx, Rx), ConnectErr>> + Send + 'static,
    HandshakeFut: Future<Output = Result<Key, HandshakeErr>> + Send + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    // The shared closures for both sides
    let handshake: Arc<Handshake<H, Key, HandshakeErr, Tx, Rx>> =
        Arc::new(move |key, chan| Box::pin(handshake(key, chan)));
    let connect: Arc<Connect<ConnectErr, Tx, Rx>> = Arc::new(move || Box::pin(connect()));
    let recover_connect = Arc::new(recover_connect);
    let recover_handshake = Arc::new(recover_handshake);

    // The shared mutable state for both sides
    let key = Arc::new(Mutex::new(None));
    let next_tx = Arc::new(Mutex::new(VecDeque::new()));
    let next_rx = Arc::new(Mutex::new(VecDeque::new()));

    let sender = Sender {
        end: End {
            key: key.clone(),
            inner: None,
            next_inner: next_tx.clone(),
            next_other: Arc::downgrade(&next_rx),
            handshake: handshake.clone(),
            connect: connect.clone(),
            recover: Box::new(recover_tx),
            recover_connect: recover_connect.clone(),
            recover_handshake: recover_handshake.clone(),
            timeout,
        },
    };

    let receiver = Receiver {
        end: End {
            key,
            inner: None,
            next_inner: next_rx,
            next_other: Arc::downgrade(&next_tx),
            handshake,
            connect,
            recover: Box::new(recover_rx),
            recover_connect,
            recover_handshake,
            timeout,
        },
    };

    S::wrap(sender, receiver)
}

/// Attempt to lock a mutex behind a weak pointer, returning `None` if its corresponding `Arc` no
/// longer exists.
async fn lock_weak<T>(weak: &Weak<Mutex<T>>) -> Option<OwnedMutexGuard<T>> {
    if let Some(arc) = weak.upgrade() {
        Some(arc.lock_owned().await)
    } else {
        None
    }
}

/// Sleep for a given duration, or until a given deadline expires, returning `true` if the sleep
/// completed before the deadline, or `false` if it did not.
///
/// This short-circuits and immediately returns `false` if it would exceed the deadline.
async fn sleep_until_or_deadline(duration: Duration, deadline: Option<Instant>) -> bool {
    let wakeup = Instant::now() + duration;
    if let Some(deadline) = deadline {
        // If we would exceed the deadline, don't wait at all
        if deadline < wakeup {
            return false;
        }
    }
    tokio::time::sleep_until(wakeup).await;
    true
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Sender<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: End<H, Key, Tx::Error, ConnectErr, HandshakeErr, Tx, Rx, Tx, Rx>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Receiver<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: End<H, Key, Rx::Error, ConnectErr, HandshakeErr, Rx, Tx, Tx, Rx>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Sender<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
{
    #[inline(always)]
    async fn tx<E>(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Tx, RetryError<E, ConnectErr, HandshakeErr>> {
        self.end.inner(|tx, rx| (tx, rx), retries, deadline).await
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Receiver<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
{
    #[inline(always)]
    async fn rx<E>(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Rx, RetryError<E, ConnectErr, HandshakeErr>> {
        self.end.inner(|tx, rx| (rx, tx), retries, deadline).await
    }
}

/// Run the retry loop for a given method on the underlying connection.
///
/// This is a macro because Rust's support for HRTBs is not sophisticated enough to typecheck the
/// higher-order function that this would need to be in order to handle all the ref cases.
///
/// It takes in something of the form `self.(tx|rx).method(x, y, ...)` and calls it in a loop until
/// either it returns successfully, or error recovery indicates it should timeout or fail.
macro_rules! retry_loop {
    ($this:tt . $field_method:tt . $($method_call:tt)*) => {{
        let mut retries = 0;
        let mut deadline = None;

        loop {
            match $this
                .$field_method(&mut retries, &mut deadline)
                .await?
                .$($method_call)*
                .await
            {
                Ok(output) => return Ok(output),
                Err(error) => {
                    if let Err(error) = $this.end.recover(&mut retries, &mut deadline, error).await
                    {
                        return Err(error);
                    }
                }
            }
        }
    }};
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> backend::Transmitter
    for Sender<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    Tx: Sync,
    Key: Sync + Send,
    Tx::Error: Send,
    ConnectErr: Send,
    HandshakeErr: Send,
{
    type Error = RetryError<Tx::Error, ConnectErr, HandshakeErr>;

    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move { retry_loop!(self.tx.send_choice(choice)) })
    }
}

#[Transmitter(Tx for ref T)]
#[Receiver(Rx)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx, T> backend::Transmit<T, Val>
    for Sender<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    T: Send + Sync + 'static,
    Tx: Sync,
    Key: Sync + Send,
    Tx::Error: Send,
    ConnectErr: Send,
    HandshakeErr: Send,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
        T: By<'a, Val>,
    {
        let message: T = call_by::to_val(message);
        Box::pin(async move { retry_loop!(self.tx.send(call_by::from_ref(&message))) })
    }
}

#[Transmitter(Tx for ref T)]
#[Receiver(Rx)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx, T> backend::Transmit<T, Ref>
    for Sender<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    for<'a> <T as By<'a, Ref>>::Type: Send,
    T: Sync + 'static,
    Tx: Sync,
    Key: Sync + Send,
    Tx::Error: Send,
    ConnectErr: Send,
    HandshakeErr: Send,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Ref>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
        T: By<'a, Ref>,
    {
        let message: &'a T = call_by::to_ref(message);
        Box::pin(async move { retry_loop!(self.tx.send(call_by::from_ref(message))) })
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> backend::Receiver
    for Receiver<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    Rx: Sync,
    Key: Sync + Send,
    Rx::Error: Send,
    ConnectErr: Send,
    HandshakeErr: Send,
{
    type Error = RetryError<Rx::Error, ConnectErr, HandshakeErr>;

    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        Box::pin(async move { retry_loop!(self.rx.recv_choice()) })
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx for T)]
impl<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx, T> backend::Receive<T>
    for Receiver<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    T: Send + 'static,
    Rx: Sync,
    Key: Sync + Send,
    Rx::Error: Send,
    ConnectErr: Send,
    HandshakeErr: Send,
{
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move { retry_loop!(self.rx.recv()) })
    }
}

/// The insides of the [`Sender`] and [`Receiver`] are almost identical, and the functionality we
/// need is almost the same. The `End` struct is the insides of both `Sender` and `Receiver`: all
/// that  differs is the choice of type parameter.
#[Transmitter(Tx)]
#[Receiver(Rx)]
struct End<H: Session, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx> {
    key: Arc<Mutex<Option<Key>>>,
    inner: Option<Inner>,
    next_inner: Arc<Mutex<VecDeque<Inner>>>,
    next_other: Weak<Mutex<VecDeque<Other>>>,
    handshake: Arc<Handshake<H, Key, HandshakeErr, Tx, Rx>>,
    connect: Arc<Connect<ConnectErr, Tx, Rx>>,
    recover: Box<dyn Fn(usize, &Err) -> RetryStrategy + Sync + Send + 'static>,
    recover_connect: Arc<dyn Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send + 'static>,
    recover_handshake:
        Arc<dyn Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send + 'static>,
    timeout: Option<Duration>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
    End<H, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
{
    /// Try to recover from a given error according to the recovery strategy specified in this
    /// `End`, otherwise returning the same error if the strategy times out or reconnection is
    /// unsuccessful.
    #[inline(always)]
    async fn recover(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
        error: Err,
    ) -> Result<(), RetryError<Err, ConnectErr, HandshakeErr>> {
        // Set the deadline if it hasn't been set already
        if *retries == 0 && deadline.is_none() {
            *deadline = self.timeout.map(|delay| Instant::now() + delay);
        }

        // Determine the error recovery strategy
        let strategy = (self.recover)(*retries, &error);

        // Drop the current `inner` if we're about to reconnect
        if matches!(strategy, RetryStrategy::ReconnectAfter(_)) {
            self.disconnect();
        }

        // Sleep until the correct deadline, or return an error if we would exceed the deadline or
        // the strategy is `Fail`
        match strategy {
            RetryStrategy::RetryAfter(after) | RetryStrategy::ReconnectAfter(after) => {
                if !sleep_until_or_deadline(after, *deadline).await {
                    return Err(RetryError::OriginalError(error));
                }
            }
            RetryStrategy::Fail => return Err(RetryError::OriginalError(error)),
        }

        // Increment retries, because we just did a recovery
        *retries += 1;

        Ok(())
    }

    /// Disconnect the inner connection of this `End`, so that future calls will require a
    /// reconnection before anything happens.
    #[inline(always)]
    fn disconnect(&mut self) {
        self.inner = None;
    }

    /// Mutably borrow the inner connection of this `End`, recreating the connection if necessary,
    /// or returning an error if it's not possible to do so within the bounds described by the
    /// reconnection strategies.
    #[inline(always)]
    async fn inner<E>(
        &mut self,
        reorder: impl Fn(Tx, Rx) -> (Inner, Other),
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Inner, RetryError<E, ConnectErr, HandshakeErr>> {
        // If there is no `inner` end currently in its slot, we need to acquire one
        if self.inner.is_none() {
            self.inner = Some(loop {
                if let Some(inner) = self.next_inner.lock().await.pop_front() {
                    // There was a waiting `inner` for us in the queue, so set it
                    break inner;
                } else {
                    // Try to reconnect, exiting the loop if successful, collecting the error if not
                    let err: Result<HandshakeErr, ConnectErr> = match (self.connect)().await {
                        Err(err) => Err(err),
                        Ok((tx, rx)) => {
                            // Take out a lock on the key (must come first to avoid deadlock)
                            let mut key = self.key.lock().await;

                            // Execute the handshake
                            let (result, ends) =
                                H::over(tx, rx, |chan| (self.handshake)(key.as_ref(), chan)).await;

                            match (result, ends) {
                                // Handshake session returned an error
                                (Err(err), _) => Ok(err),
                                // Handshake session was incomplete
                                (Ok(_), Err(_)) => return Err(RetryError::HandshakeIncomplete),
                                // Handshake session succeeded
                                (Ok(new_key), Ok((tx, rx))) => {
                                    // Depending on whether we're in a `Receiver` or `Sender`, swap
                                    // the tx/rx for each other to get an inner/other pair
                                    let (inner, other) = reorder(tx, rx);
                                    // Put the new `rx` on the other's queue
                                    if let Some(mut q) = lock_weak(&self.next_other).await {
                                        q.push_back(other);
                                    }
                                    // Set the new key (must come last to avoid deadlock)
                                    *key = Some(new_key);
                                    // Set the new `inner`
                                    break inner;
                                }
                            }
                        }
                    };

                    // Set the deadline after the first error occurs
                    if *retries == 0 && deadline.is_none() {
                        *deadline = self.timeout.map(|delay| Instant::now() + delay);
                    }

                    // Recover from this error, based on the strategy given by
                    // `recover_connect`/`recover_handshake` (whichever is applicable)
                    let (strategy, err) = match err {
                        Ok(err) => (
                            (self.recover_handshake)(*retries, &err),
                            RetryError::HandshakeError(err),
                        ),
                        Err(err) => (
                            (self.recover_connect)(*retries, &err),
                            RetryError::ConnectError(err),
                        ),
                    };
                    match strategy {
                        ReconnectStrategy::Fail => return Err(err),
                        ReconnectStrategy::ReconnectAfter(after) => {
                            if !sleep_until_or_deadline(after, *deadline).await {
                                return Err(err);
                            }
                        }
                    }
                };

                // Increase the count, so next iteration we can recover differently
                *retries += 1;
            });
        }

        // No matter what, if execution reaches this point, `self.tx.is_some()` will hold
        Ok(self.inner.as_mut().unwrap())
    }
}
