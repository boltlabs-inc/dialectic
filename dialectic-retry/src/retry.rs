use dialectic::{
    backend::{self, By},
    prelude::*,
};
use std::{future::Future, marker::PhantomData, pin::Pin, sync::Arc, time::Duration};
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

type Handshake<H, Key, Err, Tx, Rx> = dyn Fn(Option<&Key>, Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<Key, Err>> + Send>>
    + Send
    + Sync;

type Connect<Err, Tx, Rx> =
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<(Tx, Rx), Err>> + Send>> + Send + Sync;

#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Clone)]
pub struct Connector<Key, ConnectErr, HandshakeErr, Tx, Rx, H, S = Session! {}>
where
    S: Session,
    H: Session,
{
    handshake: Arc<Handshake<H, Key, HandshakeErr, Tx, Rx>>,
    recover_connect: Arc<dyn Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send>,
    recover_handshake: Arc<dyn Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send>,
    recover_tx: Arc<dyn Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send>,
    recover_rx: Arc<dyn Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send>,
    timeout: Option<Duration>,
    session: PhantomData<S>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Connector<Key, ConnectErr, HandshakeErr, Tx, Rx, H, Session! {}>
where
    H: Session,
    Key: Clone + Sync + Send + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    /// Create a new [`Connector`] which connects using the specified handshake.
    ///
    /// By default, a [`Connector`] produces channels with the empty session type, and will not
    /// attempt to retry connections. Use its various builder methods to add recovery strategies and
    /// alter the default session type.
    pub fn new<HandshakeFut>(
        handshake: impl Fn(Option<&Key>, Chan<H, Tx, Rx>) -> HandshakeFut + Sync + Send + 'static,
    ) -> Self
    where
        HandshakeFut: Future<Output = Result<Key, HandshakeErr>> + Send + 'static,
    {
        Self {
            handshake: Arc::new(move |key, chan| Box::pin(handshake(key, chan))),
            recover_connect: Arc::new(|_, _| ReconnectStrategy::Fail),
            recover_handshake: Arc::new(|_, _| ReconnectStrategy::Fail),
            recover_tx: Arc::new(|_, _| RetryStrategy::Fail),
            recover_rx: Arc::new(|_, _| RetryStrategy::Fail),
            timeout: None,
            session: PhantomData,
        }
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H, S, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Connector<Key, ConnectErr, HandshakeErr, Tx, Rx, H, S>
where
    S: Session,
    H: Session,
    Key: Clone + Sync + Send + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    /// Set the session type for all future [`Chan`]s produced by this [`Connector`].
    pub fn session<P: Session>(self) -> Connector<Key, ConnectErr, HandshakeErr, Tx, Rx, H, P> {
        let Connector {
            handshake,
            recover_connect,
            recover_handshake,
            recover_tx,
            recover_rx,
            timeout,
            ..
        } = self;
        Connector {
            handshake,
            recover_connect,
            recover_handshake,
            recover_tx,
            recover_rx,
            timeout,
            session: PhantomData,
        }
    }

    /// Set the recovery method for connection errors within all future [`Chan`]s produced by this
    /// [`Connector`].
    pub fn recover_connect(
        mut self,
        recovery: impl Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send + 'static,
    ) -> Self {
        self.recover_connect = Arc::new(recovery);
        self
    }

    /// Set the recovery method for handshake errors within all future [`Chan`]s produced by this
    /// [`Connector`].
    pub fn recover_handshake(
        mut self,
        recovery: impl Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send + 'static,
    ) -> Self {
        self.recover_handshake = Arc::new(recovery);
        self
    }

    /// Set the recovery method for transmitter errors within all future [`Chan`]s produced by this
    /// [`Connector`].
    pub fn recover_tx(
        mut self,
        recovery: impl Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send + 'static,
    ) -> Self {
        self.recover_tx = Arc::new(recovery);
        self
    }

    /// Set the recovery method for receiver errors within all future [`Chan`]s produced by this
    /// [`Connector`].
    pub fn recover_rx(
        mut self,
        recovery: impl Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send + 'static,
    ) -> Self {
        self.recover_rx = Arc::new(recovery);
        self
    }

    /// Set a timeout for recovery within all future [`Chan`]s produced by this [`Connector`]: an
    /// error will be thrown if recovery from an error takes longer than the given timeout, even if
    /// the error recovery strategy specifies trying again.
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Clear the timeout for recovery within all future [`Chan`]s produced by this [`Connector`]:
    /// recovery attempts will continue indefinitely until the recovery strategy indicates that they
    /// should stop.
    pub fn clear_timeout(mut self) -> Self {
        self.timeout = None;
        self
    }

    /// Create a new [`Chan`] which will connect to some underlying transport `(Tx, Rx)` using the
    /// specified closure, using the current configuration of this [`Connector`].
    ///
    /// This will not immediately call the connection closure; instead, this closure is called, and
    /// the connection established, when the first [`send`](Transmit::send) or
    /// [`recv`](Receive::recv) operation is performed on the [`Chan`].
    pub async fn connect<ConnectFut>(
        &self,
        connect: impl Fn() -> ConnectFut + Sync + Send + 'static,
    ) -> Chan<
        S,
        Sender<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
        Receiver<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
    >
    where
        ConnectFut: Future<Output = Result<(Tx, Rx), ConnectErr>> + Send + 'static,
    {
        // The shared connect closure for both sides
        let connect: Arc<Connect<ConnectErr, Tx, Rx>> = Arc::new(move || Box::pin(connect()));

        let (sender, receiver) = end::channel(
            connect,
            self.handshake.clone(),
            self.recover_connect.clone(),
            self.recover_handshake.clone(),
            self.recover_tx.clone(),
            self.recover_rx.clone(),
            self.timeout,
        );

        S::wrap(Sender { end: sender }, Receiver { end: receiver })
    }
}

mod end;
use end::{ReceiverEnd, SenderEnd};

/// The transmitting end of a retrying connection.
#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Sender<H: Session, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: SenderEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
}

/// The receiving end of a retrying connection.
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
    /// Get a mutable reference to the inner transmitter end, or fail after some number of retry
    /// attempts if the retry strategies indicate so.
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
    /// Get a mutable reference to the inner receiver end, or fail after some number of retry
    /// attempts if the retry strategies indicate so.
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
