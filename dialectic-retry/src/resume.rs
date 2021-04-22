use dashmap::DashMap;
use dialectic::{
    backend::{self, By},
    prelude::*,
};
use std::{
    fmt::{Debug, Display},
    future::Future,
    hash::Hash,
    marker::PhantomData,
    pin::Pin,
    sync::{atomic::AtomicBool, Arc},
    time::Duration,
};
use tokio::sync::mpsc;

/// An error while accepting a connection with an [`Acceptor`].
#[derive(Debug, Clone)]
pub enum AcceptError<Key, Err> {
    /// An error was returned during the session initialization handshake.
    HandshakeError(Err),
    /// The session initialization handshake did not appropriately complete its session type.
    HandshakeIncomplete,
    /// The session initialization handshake returned a session key marked for resumption of an
    /// existing session, but no corresponding session exists for this key.
    ///
    /// This error always indicates a programmer error in the session handshake with which the
    /// [`Acceptor`] was created.
    NoSuchSessionKey(Key),
    /// The session initialization handshake returned a session key marked for creation of a new
    /// session, but a session with this key already exists.
    SessionKeyAlreadyExists(Key),
    /// The buffer for this session key is full of resumed connections which have not yet been
    /// processed by the connected [`Sender`]/[`struct@Receiver`].
    ///
    /// This error usually only occurs if the retrying end is misbehaving.
    NoCapacity,
}

impl<Key: Display, Err: Display> Display for AcceptError<Key, Err> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AcceptError::*;
        match self {
            HandshakeError(e) => write!(f, "{}", e),
            HandshakeIncomplete => write!(f, "handshake session incomplete"),
            NoSuchSessionKey(key) => write!(f, "no session exists for key: {}", key),
            SessionKeyAlreadyExists(key) => write!(f, "a session already exists for key: {}", key),
            NoCapacity => write!(f, "no capacity to add pending session"),
        }
    }
}

impl<Key: Display + Debug, Err: Display + Debug> std::error::Error for AcceptError<Key, Err> {}

/// An error within a resume-enabled connection.
#[derive(Debug, Clone)]
pub enum ResumeError<Err> {
    /// An error occurred in the underlying connection which was not able to be recovered by the
    /// resumption strategy for this [`Sender`] or [`struct@Receiver`].
    Error(Err),
    /// The timeout expired for the retrying end to reconnect.
    ///
    /// This error usually only occurs if a method is called on a [`Sender`] or [`struct@Receiver`]
    /// after that [`Sender`] or [`struct@Receiver`] returns an error.
    ConnectTimeout,
}

impl<Err: Display> Display for ResumeError<Err> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use ResumeError::*;
        match self {
            Error(e) => write!(f, "{}", e),
            ConnectTimeout => write!(f, "timeout while connecting"),
        }
    }
}

impl<Err: Display + Debug> std::error::Error for ResumeError<Err> {}

/// A description of what to do when an error happens in a resume-enabled connection.
#[derive(Debug, Clone, Copy)]
pub enum ResumeStrategy {
    /// Pause for this duration, then retry the operation on the same underlying connection.
    RetryAfter(Duration),
    /// Immediately discard the underlying connection and then retry the operation once a new
    /// connection has been established.
    Reconnect,
    /// Immediately fail, propagating the current error.
    Fail,
}

impl Default for ResumeStrategy {
    fn default() -> Self {
        Self::Fail
    }
}

/// The kind of a connection which a handshake session indicates should be created.
pub enum ResumeKind {
    /// Create a new [`Chan`] for this connection.
    New,
    /// Resume an existing session using this connection.
    Existing,
}

/// The "feed lines" to a matched pair of [`Sender`] and [`struct@Receiver`] so that the [`Acceptor`] which
/// controls them can supply them with newly accepted transmitter and receiver connections.
pub(crate) struct FeedLine<Tx, Rx> {
    /// Sender for transmitter ends.
    send_tx: mpsc::Sender<Tx>,
    /// Sender for receiver ends.
    send_rx: mpsc::Sender<Rx>,
    /// Indicator noting whether at least one of the [`Sender`] or [`struct@Receiver`] is dropped.
    ///
    /// In the [`Drop`] impl, this is used to remove a key's entry from the managed map once both
    /// its [`Sender`] and [`struct@Receiver`] have dropped.
    half_dropped: AtomicBool,
}

/// A set of [`FeedLine`]s to [`Sender`]/[`struct@Receiver`] pairs, indexed by `Key` and accessible
/// concurrently.
type Managed<Key, Tx, Rx> = DashMap<Key, FeedLine<Tx, Rx>>;

/// A function which executes an [`Acceptor`]-side handshake.
type Handshake<H, Key, E, Tx, Rx> =
    dyn Fn(Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<(ResumeKind, Key), E>>>>;

/// An [`Acceptor`] knows how to accept an incoming pair of transmitter `Tx` and receiver `Rx` and
/// perform a handshake to determine whether they intend to create a new connection or resume an
/// existing one. It manages a set of [`Chan`]s, each created by a call to
/// [`accept`](Acceptor::accept), so that if an error occurs in some [`Chan`] it manages, the other
/// end of the connection can retry and be reconnected to the same [`Chan`], without the user of the
/// [`Chan`] being aware that the retry/resume occurred.
///
/// An [`Acceptor<Key, Err, Tx, Rx, H, S>`](Acceptor) is meant to be used as the counterpart to a
/// [`Connector<Key, ConnectErr, HandshakeErr, Tx, Rx, H::Dual, S::Dual>`](crate::retry::Connector).
/// If the handshake session type `H` and regular session type `S` are not both respectively duals
/// between the [`Acceptor`] and the [`Connector`](crate::retry::Connector), errors will occur.
#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Acceptor<Key, Err, Tx, Rx, H, S>
where
    H: Session,
    S: Session,
{
    handshake: Box<Handshake<H, Key, Err, Tx, Rx>>,
    managed: Arc<Managed<Key, Tx, Rx>>,
    recover_tx: Arc<dyn Fn(usize, &Tx::Error) -> ResumeStrategy + Sync + Send>,
    recover_rx: Arc<dyn Fn(usize, &Rx::Error) -> ResumeStrategy + Sync + Send>,
    timeout: Option<Duration>,
    buffer_size: usize,
    session: PhantomData<fn() -> S>,
}

mod end;
use end::{End, ReceiverEnd, SenderEnd};

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, H, Err, Tx, Rx> Acceptor<Key, Err, Tx, Rx, H, Session! {}>
where
    Key: Clone + Eq + Hash + Send + Sync + 'static,
    H: Session,
{
    /// Create a new [`Acceptor`] which accepts connections using the specified handshake.
    ///
    /// By default, an [`Acceptor`] produces channels with the empty session type, no timeout, a
    /// buffer size of 1, and will attempt to resume connections indefinitely. Use its various
    /// builder methods to add recovery strategies and alter the default session type, timeout, and
    /// buffer size.
    pub fn new<Fut>(handshake: impl Fn(Chan<H, Tx, Rx>) -> Fut + 'static) -> Self
    where
        Fut: Future<Output = Result<(ResumeKind, Key), Err>> + 'static,
    {
        Self {
            handshake: Box::new(move |chan| Box::pin(handshake(chan))),
            managed: Arc::new(DashMap::new()),
            recover_tx: Arc::new(|_, _| ResumeStrategy::Reconnect),
            recover_rx: Arc::new(|_, _| ResumeStrategy::Reconnect),
            timeout: None,
            buffer_size: 1,
            session: PhantomData,
        }
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, H, Err, S, Tx, Rx> Acceptor<Key, Err, Tx, Rx, H, S>
where
    Key: Clone + Eq + Hash + Send + Sync + 'static,
    H: Session,
    S: Session,
{
    /// Set the session type for all future [`Chan`]s produced by this [`Acceptor`].
    pub fn session<P: Session>(self) -> Acceptor<Key, Err, Tx, Rx, H, P> {
        let Acceptor {
            handshake,
            managed,
            recover_tx,
            recover_rx,
            timeout,
            buffer_size,
            ..
        } = self;
        Acceptor {
            handshake,
            managed,
            recover_tx,
            recover_rx,
            timeout,
            buffer_size,
            session: PhantomData,
        }
    }

    /// Set the recovery method for transmitter errors within all future [`Chan`]s produced by this
    /// [`Acceptor`].
    pub fn recover_tx(
        mut self,
        recovery: impl Fn(usize, &Tx::Error) -> ResumeStrategy + Sync + Send + 'static,
    ) -> Self {
        self.recover_tx = Arc::new(recovery);
        self
    }

    /// Set the recovery method for receiver errors within all future [`Chan`]s produced by this
    /// [`Acceptor`].
    pub fn recover_rx(
        mut self,
        recovery: impl Fn(usize, &Rx::Error) -> ResumeStrategy + Sync + Send + 'static,
    ) -> Self {
        self.recover_rx = Arc::new(recovery);
        self
    }

    /// Set a timeout for recovery within all future [`Chan`]s produced by this [`Acceptor`].
    ///
    /// When there is a timeout, an error will be thrown if recovery from a previous error takes
    /// longer than the given timeout, even if the error recovery strategy specifies trying again.
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Clear the timeout for recovery within all future [`Chan`]s produced by this [`Acceptor`].
    ///
    /// When there is no timeout,  recovery attempts will continue indefinitely until the recovery
    /// strategy indicates that they should stop.
    pub fn clear_timeout(mut self) -> Self {
        self.timeout = None;
        self
    }

    /// Set the buffer size for all future [`Chan`]s produced by this [`Acceptor`].
    ///
    /// If on an [`accept`](Acceptor::accept), more than `buffer_size` number of successful retries
    /// are waiting for a [`Chan`], an error will be thrown instead of a successful resumption.
    pub fn buffer_size(mut self, buffer_size: usize) -> Self {
        self.buffer_size = buffer_size.saturating_add(1);
        self
    }

    /// Attempt to accept a new connection using this [`Acceptor`].
    ///
    /// If this returns `Ok((key, None))`, the connection has been successfully forwarded to some
    /// existing pair of [`Sender`] and [`struct@Receiver`] because the handshake returned a
    /// matching key for that session. If this returns `Ok((key, Some(chan)))`, the handshake
    /// successfully indicated that a new session should be started for the returned key. Otherwise,
    /// an error occurred while trying to accept this connection, and is returned.
    pub async fn accept(
        &self,
        tx: Tx,
        rx: Rx,
    ) -> Result<
        (
            Key,
            Option<Chan<S, Sender<Key, Tx, Rx>, Receiver<Key, Tx, Rx>>>,
        ),
        AcceptError<Key, Err>,
    > {
        let (result, ends) = H::over(tx, rx, |chan| (self.handshake)(chan)).await;
        let (connect_kind, key) = match result {
            Ok(key) => key,
            Err(error) => return Err(AcceptError::HandshakeError(error)),
        };
        let (tx, rx) = match ends {
            Ok(ends) => ends,
            Err(_) => return Err(AcceptError::HandshakeIncomplete),
        };
        match (connect_kind, self.managed.get_mut(&key)) {
            (ResumeKind::Existing, None) => {
                // If the connection was supposed to be for an existing key but no key
                // existed yet, return an error instead of generating a new channel.
                Err(AcceptError::NoSuchSessionKey(key))
            }
            (ResumeKind::New, Some(_)) => {
                // If the connection was supposed to be for a new key, but the key existed,
                // return an error instead of resuming on that key.
                Err(AcceptError::SessionKeyAlreadyExists(key))
            }
            (ResumeKind::Existing, Some(map_ref)) => {
                // If there was already an entry by this key, there's a session, potentially
                // paused, by this key. If the handshake was properly implemented, we're
                // only receiving this tx/rx pair because the other end tried to reconnect,
                // which means that the connection broke in one direction or another, so
                // either we are currently paused, or we're about to discover the issue and
                // pause. Sending the tx/rx will un-pause us and allow us to resume on those
                // fresh channels.
                let send_tx = &map_ref.send_tx;
                let send_rx = &map_ref.send_rx;

                use mpsc::error::TrySendError::{Closed, Full};
                if (send_tx.capacity() > 0 || send_tx.is_closed())
                    && (send_rx.capacity() > 0 || send_rx.is_closed())
                {
                    match (send_tx.try_send(tx), send_rx.try_send(rx)) {
                        // This should be impossible, because we have exclusive access due to the
                        // map reference, and nobody else ever sends, so capacity can only increase:
                        (Err(Full(_)), Ok(())) | (Ok(()), Err(Full(_))) => {
                            panic!("partial resumption after capacity check")
                        }
                        // This should also be impossible, but it's less bad if it happens because
                        // if the other side is also full/closed, there's no mismatch that would
                        // lead to the `Sender` having a `tx` that doesn't correspond to the
                        // `Receiver`'s `rx`
                        (Err(Full(_)), Err(Closed(_)))
                        | (Err(Closed(_)), Err(Full(_)))
                        | (Err(Full(_)), Err(Full(_)))
                        // If both are successful, or closed, then everything's fine
                        | (Ok(()), Ok(()))
                        | (Ok(()), Err(Closed(_)))
                        | (Err(Closed(_)), Ok(()))
                        | (Err(Closed(_)), Err(Closed(_))) => Ok((key, None)),
                    }
                } else {
                    Err(AcceptError::NoCapacity)
                }
            }
            (ResumeKind::New, None) => {
                // If there was no entry by this key, then this represents a fresh session,
                // and we should return a channel for the caller to do with what they like
                // (usually, run a session in a separate task).
                let (send_tx, next_tx) = mpsc::channel(self.buffer_size);
                let (send_rx, next_rx) = mpsc::channel(self.buffer_size);
                self.managed.insert(
                    key.clone(),
                    FeedLine {
                        send_tx,
                        send_rx,
                        half_dropped: false.into(),
                    },
                );
                let tx = Sender {
                    end: End::new(
                        key.clone(),
                        tx,
                        next_tx,
                        self.recover_tx.clone(),
                        self.managed.clone(),
                        self.timeout,
                    ),
                };
                let rx = Receiver {
                    end: End::new(
                        key.clone(),
                        rx,
                        next_rx,
                        self.recover_rx.clone(),
                        self.managed.clone(),
                        self.timeout,
                    ),
                };
                Ok((key, Some(S::wrap(tx, rx))))
            }
        }
    }
}

// Retry a channel operation in a loop until the error recovery routine indicates it's time to stop.
//
// This is a macro because as far as I can tell it's not possible to express as a HRTB the bounds
// necessary to quantify over the action to be performed, in the ref case.
macro_rules! retry_loop {
    ($this:tt . $($action:tt)*) => {{
        let mut retries = 0;
        let mut deadline = None;
        let mut latest_error = None;

        loop {
            let inner = match $this.end.inner(deadline).await {
                Some(inner) => inner,
                None => return Err(
                    latest_error
                        .map(ResumeError::Error)
                        .unwrap_or(ResumeError::ConnectTimeout)
                ),
            };

            match inner.$($action)*.await {
                Ok(output) => break Ok(output),
                Err(error) => {
                    if $this.end.recover(&mut retries, &mut deadline, &error).await {
                        latest_error = Some(error);
                    } else {
                        break Err(ResumeError::Error(error));
                    }
                }
            }
        }
    }};
}

/// A resuming transmitter end, wrapping a transmitter from some underlying transport backend.
///
/// The only way to create a [`Sender`] is to use [`Acceptor::accept`] and for the accepted
/// connection to specify that it wants to initiate a new connection.
#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Sender<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    end: SenderEnd<Key, Tx, Rx>,
}

/// A resuming receiver end, wrapping a receiver from some underlying transport backend.
///
/// The only way to create a [`Sender`] is to use [`Acceptor::accept`] and for the accepted
/// connection to specify that it wants to initiate a new connection.
#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Receiver<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    end: ReceiverEnd<Key, Tx, Rx>,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Tx, Rx> Transmitter for Sender<Key, Tx, Rx>
where
    Key: Sync + Send + Eq + Hash,
    Tx::Error: Send + Sync,
{
    type Error = ResumeError<Tx::Error>;

    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move { retry_loop!(self.send_choice(choice)) })
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
        Box::pin(async move { retry_loop!(self.send(&message)) })
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
        Box::pin(async move { retry_loop!(self.send(call_by::from_ref::<T>(message))) })
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Tx, Rx> backend::Receiver for Receiver<Key, Tx, Rx>
where
    Key: Send + Sync + Eq + Hash,
    Rx::Error: Send + Sync,
{
    type Error = ResumeError<Rx::Error>;

    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        Box::pin(async move { retry_loop!(self.recv_choice::<LENGTH>()) })
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
        Box::pin(async move { retry_loop!(self.recv()) })
    }
}
