//! The *retry* end of a reconnectable backend consists of a [`Connector`] and the set of [`Chan`]s
//! spawned from it with [`connect`](Connector::connect), which automatically reconnect to their
//! other side when they encounter an error.

use dialectic::{
    backend::{self, By},
    prelude::*,
};
use std::{
    fmt::{Debug, Display},
    future::Future,
    marker::PhantomData,
    pin::Pin,
    sync::Arc,
    time::Duration,
};
use tokio::time::Instant;

/// A description of what to do when an error happens that we might want to recover from.
#[derive(Debug, Clone, Copy)]
pub enum RetryStrategy {
    /// Pause for this amount of time, then retry without discarding the current connection.
    RetryAfter(Duration),
    /// Discard the current connection, then retry after this amount of time.
    ReconnectAfter(Duration),
    /// Immediately fail, propagating the error.
    Fail,
}

impl Default for RetryStrategy {
    fn default() -> Self {
        Self::Fail
    }
}

/// A description of what to do when an error happens during reconnection.
#[derive(Debug, Clone, Copy)]
pub enum ReconnectStrategy {
    /// Pause for this amount of time, then attempt to reconnect again.
    ReconnectAfter(Duration),
    /// Immediately fail, propagating the error.
    Fail,
}

impl Default for ReconnectStrategy {
    fn default() -> Self {
        Self::Fail
    }
}

/// An error while trying or retrying to perform an operation over a retry-able connection.
#[derive(Debug, Clone, Copy)]
pub enum RetryError<Err, ConnectError, HandshakeError> {
    /// An error occurred in the underlying connection.
    OriginalError(Err),
    /// An error occurred while trying to connect.
    ConnectError(ConnectError),
    /// The maximum reconnect timeout was exceeded while trying to connect.
    ConnectTimeout,
    /// An error occurred while performing the session initialization handshake.
    HandshakeError(HandshakeError),
    /// The maximum reconnect timeout was exceeded while trying to perform the session
    /// initialization handshake.
    HandshakeTimeout,
    /// The session initialization handshake did not appropriately complete its session type.
    HandshakeIncomplete,
}

impl<Err: Display, ConnectError: Display, HandshakeError: Display> Display
    for RetryError<Err, ConnectError, HandshakeError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RetryError::*;
        match self {
            OriginalError(e) => write!(f, "{}", e),
            ConnectError(e) => write!(f, "{}", e),
            ConnectTimeout => write!(f, "Timeout during connection"),
            HandshakeError(e) => write!(f, "{}", e),
            HandshakeTimeout => write!(f, "Timeout during handshake"),
            HandshakeIncomplete => write!(f, "Handshake incomplete"),
        }
    }
}

impl<Err, ConnectErr, HandshakeErr> std::error::Error for RetryError<Err, ConnectErr, HandshakeErr>
where
    Err: Display + Debug,
    ConnectErr: Display + Debug,
    HandshakeErr: Display + Debug,
{
}

/// A function which executes a [`Connector`]-side handshake.
type Init<H, Key, Err, Tx, Rx> =
    dyn Fn(Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<Key, Err>> + Send>> + Send + Sync;

type Retry<H, Key, Err, Tx, Rx> = dyn Fn(Key, Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<(), Err>> + Send>>
    + Send
    + Sync;

/// A function which creates a new connection.
type ConnectTo<Addr, Err, Tx, Rx> =
    dyn Fn(Addr) -> Pin<Box<dyn Future<Output = Result<(Tx, Rx), Err>> + Send>> + Send + Sync;

/// A [`Connector`] is a builder for retrying connections, which are parameterized by strategies
/// describing how and when to recover from errors in the underlying transport. When a connection is
/// created or re-created, a handshake occurs to indicate to the other side of the connection the
/// identity of this session, so that the other side can pick up where it left off.
///
/// A [`Connector<Key, ConnectErr, HandshakeErr, Tx, Rx, H, S>`](Connector) is meant to be used as
/// the counterpart to a [`Acceptor<Key, Err, Tx, Rx, H::Dual, S::Dual>`](crate::resume::Acceptor).
/// If the handshake session type `H` and regular session type `S` are not both respectively duals
/// between the [`Acceptor`](crate::resume::Acceptor) and the [`Connector`], errors will occur.
#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct Connector<Address, Key, ConnectErr, HandshakeErr, Tx, Rx, H, S>
where
    S: Session,
    H: Session,
{
    /// An async function to create a new connection, or return an error.
    #[derivative(Debug = "ignore")]
    connect: Arc<ConnectTo<Address, ConnectErr, Tx, Rx>>,
    /// An async function to perform an initial handshake.
    #[derivative(Debug = "ignore")]
    init: Arc<Init<H, Key, HandshakeErr, Tx, Rx>>,
    /// An async function to perform an initial handshake.
    #[derivative(Debug = "ignore")]
    retry: Arc<Retry<H, Key, HandshakeErr, Tx, Rx>>,
    /// An async function to recover after a connection failure.
    #[derivative(Debug = "ignore")]
    recover_connect: Arc<dyn Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send>,
    /// An async function to recover after a handshake error.
    #[derivative(Debug = "ignore")]
    recover_handshake: Arc<dyn Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send>,
    /// An async function to recover after a `Tx` error.
    #[derivative(Debug = "ignore")]
    recover_tx: Arc<dyn Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send>,
    /// An async function to recover after an `Rx` error.
    #[derivative(Debug = "ignore")]
    recover_rx: Arc<dyn Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send>,
    /// An optional timeout, which bounds all retry attempts.
    timeout: Option<Duration>,
    /// The session type of all channels created by this connector.
    session: PhantomData<S>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Connector<Address, Key, ConnectErr, HandshakeErr, Tx, Rx, H, Session! {}>
where
    H: Session,
    Key: Clone + Sync + Send + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    /// Create a new [`Connector`] which connects using the `connect` closure and the specified
    /// handshakes, using the `init` handshake when first creating channels, and the `retry`
    /// handshake when retrying after errors.
    ///
    /// By default, a [`Connector`] produces channels with the empty session type, no timeout, and
    /// will not attempt to retry connections (all errors will immediately propagate). Use its
    /// various builder methods to add recovery strategies and alter the default session type and
    /// timeout.
    pub fn new<ConnectFut, InitFut, RetryFut>(
        connect: impl Fn(Address) -> ConnectFut + Sync + Send + 'static,
        init: impl Fn(Chan<H, Tx, Rx>) -> InitFut + Sync + Send + 'static,
        retry: impl Fn(Key, Chan<H, Tx, Rx>) -> RetryFut + Sync + Send + 'static,
    ) -> Self
    where
        ConnectFut: Future<Output = Result<(Tx, Rx), ConnectErr>> + Send + 'static,
        InitFut: Future<Output = Result<Key, HandshakeErr>> + Send + 'static,
        RetryFut: Future<Output = Result<(), HandshakeErr>> + Send + 'static,
    {
        Self {
            connect: Arc::new(move |addr| Box::pin(connect(addr))),
            init: Arc::new(move |chan| Box::pin(init(chan))),
            retry: Arc::new(move |key, chan| Box::pin(retry(key, chan))),
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
impl<H, S, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Connector<Address, Key, ConnectErr, HandshakeErr, Tx, Rx, H, S>
where
    S: Session,
    H: Session,
    Key: Clone + Sync + Send + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    /// Set the session type for all future [`Chan`]s produced by this [`Connector`].
    pub fn session<P: Session>(
        self,
    ) -> Connector<Address, Key, ConnectErr, HandshakeErr, Tx, Rx, H, P> {
        let Connector {
            connect,
            init,
            retry,
            recover_connect,
            recover_handshake,
            recover_tx,
            recover_rx,
            timeout,
            ..
        } = self;
        Connector {
            connect,
            init,
            retry,
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
    pub async fn connect(
        &self,
        address: Address,
    ) -> Result<
        (
            Key,
            Chan<
                S,
                Sender<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>,
                Receiver<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>,
            >,
        ),
        RetryError<std::convert::Infallible, ConnectErr, HandshakeErr>,
    >
    where
        Address: Clone + Sync + Send + 'static,
    {
        let (sender, receiver) = end::channel(
            address,
            self.connect.clone(),
            self.init.clone(),
            self.retry.clone(),
            Recoveries {
                connect: self.recover_connect.clone(),
                handshake: self.recover_handshake.clone(),
                tx: self.recover_tx.clone(),
                rx: self.recover_rx.clone(),
            },
            self.timeout,
        );

        let mut tx = Sender { end: sender };
        let rx = Receiver { end: receiver };
        let key = tx.end.key(|tx, rx| (tx, rx)).await?;

        Ok((key, S::wrap(tx, rx)))
    }
}

mod end;
use end::{ReceiverEnd, Recoveries, SenderEnd};

/// The transmitting end of a retrying connection, wrapping a transmitter from some underlying
/// transport backend.
///
/// The only way to create a [`Sender`] is to use [`Connector::connect`].
#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Debug)]
pub struct Sender<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: SenderEnd<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>,
}

/// The receiving end of a retrying connection, wrapping a receiver from some underlying transport
/// backend.
///
/// The only way to create a [`struct@Receiver`] is to use [`Connector::connect`].
#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Debug)]
pub struct Receiver<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx> {
    end: ReceiverEnd<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Sender<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
{
    /// Get a mutable reference to the inner transmitter end, or fail after some number of retry
    /// attempts if the retry strategies indicate so.
    #[inline(always)]
    async fn tx<E>(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Tx, RetryError<E, ConnectErr, HandshakeErr>>
    where
        Address: Clone,
        Key: Clone,
    {
        self.end.inner(|tx, rx| (tx, rx), retries, deadline).await
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
    Receiver<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
{
    /// Get a mutable reference to the inner receiver end, or fail after some number of retry
    /// attempts if the retry strategies indicate so.
    #[inline(always)]
    async fn rx<E>(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Rx, RetryError<E, ConnectErr, HandshakeErr>>
    where
        Address: Clone,
        Key: Clone,
    {
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
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx> Transmitter
    for Sender<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    Tx: Sync,
    Address: Clone + Sync + Send,
    Key: Clone + Sync + Send,
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
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx, T> Transmit<T, Val>
    for Sender<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    T: Send + Sync + 'static,
    Tx: Sync,
    Address: Clone + Sync + Send,
    Key: Clone + Sync + Send,
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
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx, T> Transmit<T, Ref>
    for Sender<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    for<'a> <T as By<'a, Ref>>::Type: Send,
    T: Sync + 'static,
    Tx: Sync,
    Address: Clone + Sync + Send,
    Key: Clone + Sync + Send,
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
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx> backend::Receiver
    for Receiver<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    Rx: Sync,
    Address: Clone + Sync + Send,
    Key: Clone + Sync + Send,
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
impl<H: Session, Address, Key, ConnectErr, HandshakeErr, Tx, Rx, T> Receive<T>
    for Receiver<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>
where
    T: Send + 'static,
    Rx: Sync,
    Address: Clone + Sync + Send,
    Key: Clone + Sync + Send,
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
