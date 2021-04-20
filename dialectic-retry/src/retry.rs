use dialectic::{
    backend::{self, By},
    prelude::*,
};
use std::{future::Future, pin::Pin, time::Duration};
use tokio::time::Instant;

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
    ConnectTimeout,
    HandshakeError(HandshakeError),
    HandshakeTimeout,
    HandshakeIncomplete,
}

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
    let (sender, receiver) = end::channel(
        connect,
        handshake,
        recover_connect,
        recover_handshake,
        recover_tx,
        recover_rx,
        timeout,
    );

    S::wrap(Sender { end: sender }, Receiver { end: receiver })
}

mod end;
use end::{ReceiverEnd, SenderEnd};

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Sender<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: SenderEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Receiver<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: ReceiverEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
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
