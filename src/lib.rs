use async_trait::async_trait;
use std::{
    any::{Any, TypeId},
    marker::{self, PhantomData},
    sync::Arc,
};

pub use types::*;

pub mod types;

/// If something is `Transmit<T>`, we can use it to [`Transmit::send`] a message of type `T`.
#[async_trait]
pub trait Transmit<T> {
    /// The type of possible errors when sending.
    type Error;

    /// Send a message.
    async fn send(&mut self, message: &T) -> Result<(), Self::Error>;
}

#[async_trait]
impl<T: Sync, C: Transmit<T> + marker::Send> Transmit<T> for &'_ mut C {
    type Error = C::Error;

    async fn send(&mut self, message: &T) -> Result<(), Self::Error> {
        (**self).send(message).await
    }
}

/// If something is `Receive<T>`, we can use it to [`Receive::recv`] a message of type `T`.
#[async_trait]
pub trait Receive<T> {
    /// The type of possible errors when receiving.
    type Error;

    /// Receive a message. This may require type annotations for disambiguation.
    async fn recv(&mut self) -> Result<T, Self::Error>;
}

#[async_trait]
impl<T: 'static, C: Receive<T> + marker::Send> Receive<T> for &'_ mut C {
    type Error = C::Error;

    async fn recv(&mut self) -> Result<T, Self::Error> {
        (**self).recv().await
    }
}

/// A session type describes the sequence of operations performed by one end of a bidirectional
/// [`Chan`]. Each session type has a [`Session::Dual`], the type of the corresponding client on the
/// other side of the channel. The sealed trait `Session` enumerates these types, and provides the
/// dual of each.
pub trait Session: Any + sealed::Session {
    /// The dual to this session type, i.e. the session type required of the other end of the
    /// channel.
    type Dual;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed `AllSession` trait ensures that every protocol in a type level list of
/// protocols is `Session`.
pub trait AllSession: sealed::AllSession {
    type AllDual;
    type Arity: Unary;
}

impl AllSession for () {
    type AllDual = ();
    type Arity = Z;
}

impl<P: Session, Ps: AllSession> AllSession for (P, Ps) {
    type AllDual = (P::Dual, Ps::AllDual);
    type Arity = S<Ps::Arity>;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed `Select` trait describes what it means to index into a type level list of
/// protocols.
pub trait Select<N>: sealed::Select<N> {
    type Selected;
}

impl<T, S> Select<Z> for (T, S) {
    type Selected = T;
}

impl<T, P, N> Select<S<N>> for (T, (P, ()))
where
    (P, ()): Select<N>,
{
    type Selected = <(P, ()) as Select<N>>::Selected;
}

/// A bidirectional communications channel sending outgoing messages via the `Tx` connection and
/// receiving incoming messages via the `Rx` connection, whose future session behavior is determined
/// by the session type `P`.
///
/// The `E` parameter describes the *session environment* of the channel: the stack of [`Loop`]s the
/// channel has entered.
///
/// # Panics
///
/// For *unfinished* channels (that is, those with a session type that is not `End`), dropping them
/// before the end of their session will result in a panic.
#[derive(Debug)]
#[must_use = "Dropping a channel before finishing its session type will result in a panic"]
pub struct Chan<Tx, Rx, P: Session, E = ()> {
    tx: Option<Tx>, // never `None` unless currently in the body of `unwrap`
    rx: Option<Rx>, // never `None` unless currently in the body of `unwrap`
    environment: PhantomData<E>,
    protocol: PhantomData<P>,
}

impl<Tx, Rx, P: Session> Chan<Tx, Rx, P> {
    /// Given a transmitting and receiving end of an un-session-typed connection, create a new
    /// channel for the protocol `P.`
    ///
    /// Because `&mut Tx` and `&mut Rx` are `Transmit<T>` and `Receive<T>` if `Tx` and `Rx` are
    /// `Transmit<T>` and `Receive<T>` respectively, a `Chan` does not need to own its connections;
    /// a mutable reference or an owned type work equally well as inputs to [`Chan::new`].
    #[must_use]
    pub fn new(tx: Tx, rx: Rx) -> Chan<Tx, Rx, P> {
        unsafe { Chan::with_env(tx, rx) }
    }
}

impl<Tx, Rx> Chan<Tx, Rx, End> {
    /// Close a finished session, returning the wrapped connections used during the session.
    ///
    /// This function does not do cleanup on the actual underlying connections; they are passed back
    /// to the caller who may either continue to use them outside the session typing context or
    /// clean them up as appropriate.
    pub fn close(self) -> (Tx, Rx) {
        self.unwrap()
    }
}

impl<Tx, Rx, P: Session, E> Drop for Chan<Tx, Rx, P, E> {
    fn drop(&mut self) {
        let mut this = std::mem::ManuallyDrop::new(self);
        drop(this.rx.take());
        drop(this.tx.take());
        if TypeId::of::<P>() != TypeId::of::<End>() {
            panic!("Channel dropped before finishing its session")
        }
    }
}

impl<'a, Tx, Rx: Receive<T>, E, T: marker::Send + Any, P: Session> Chan<Tx, Rx, Recv<T, P>, E> {
    /// Receive something of type `T` on the channel, returning the pair of the received object and
    /// the channel.
    ///
    /// This function returns the [`Receive::Error`] for the underlying `Rx` connection if there was
    /// an error while receiving.
    #[must_use]
    pub async fn recv(mut self) -> Result<(T, Chan<Tx, Rx, P, E>), Rx::Error> {
        match self.rx().recv().await {
            Ok(result) => Ok((result, unsafe { self.cast() })),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

impl<'a, Tx: Transmit<T>, Rx, E, T: marker::Send + Any, P: Session> Chan<Tx, Rx, Send<T, P>, E> {
    /// Send something of type `T` on the channel, returning the channel.
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending.
    #[must_use]
    pub async fn send(mut self, message: &T) -> Result<Chan<Tx, Rx, P, E>, Tx::Error> {
        match self.tx().send(message).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

impl<'a, Tx: Transmit<usize>, Rx, E, Ps: AllSession> Chan<Tx, Rx, Choose<Ps>, E> {
    /// Actively choose to enter the `N`th protocol offered via [`Chan::offer`] by the other end of
    /// the connection, alerting the other party to this choice by sending the number `N` over the
    /// channel.
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending the choice.
    #[must_use]
    pub async fn choose<N: Unary>(mut self) -> Result<Chan<Tx, Rx, Ps::Selected, E>, Tx::Error>
    where
        Ps: Select<N>,
        Ps::Selected: Session,
    {
        match self.tx().send(&N::VALUE).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

/// The result of [`Chan::offer`], `Branches<Tx, Rx, Ps>` represents an `N`-ary enumeration of the
/// possible new channel states that could result from the offering of `N` protocols.
///
/// To find out which protocol was selected by the other party, use [`Branches::case`], or better
/// yet, use the `offer!` macro to ensure you don't miss any cases.
#[derive(Debug)]
#[must_use]
pub struct Branches<Tx, Rx, Ps: AllSession, E = ()> {
    variant: usize,
    tx: Tx,
    rx: Rx,
    protocols: PhantomData<Ps>,
    environment: PhantomData<E>,
}

impl<'a, Tx, Rx, P: Session, Ps: AllSession, E> Branches<Tx, Rx, (P, Ps), E> {
    /// Check if the selected protocol in this [`Branches`] was `P`. If so, return the corresponding
    /// channel; otherwise, return all the other possibilities.
    #[must_use = "all possible choices must be handled (add cases to match the type of this `Offer<...>`)"]
    pub fn case(self) -> Result<Chan<Tx, Rx, P, E>, Branches<Tx, Rx, Ps, E>> {
        if self.variant == 0 {
            Ok(unsafe { Chan::with_env(self.tx, self.rx) })
        } else {
            Err(Branches {
                variant: self.variant - 1,
                tx: self.tx,
                rx: self.rx,
                protocols: PhantomData,
                environment: PhantomData,
            })
        }
    }
}

impl<'a, Tx, Rx, E> Branches<Tx, Rx, (), E> {
    /// It is impossible to construct a `Branches` with no options; this function eliminates that
    /// case.
    pub fn empty_case<T>(self) -> T {
        unreachable!("Impossible case on non-constructible empty `Branches`")
    }
}

impl<'a, Tx, Rx: Receive<usize>, P: Session, Ps: AllSession, E> Chan<Tx, Rx, Offer<(P, Ps)>, E> {
    /// Offer the choice of one or more protocols to the other party, and wait for them to indicate
    /// by sending a number which protocol to proceed with.
    ///
    /// It's recommended to use the `offer!` macro to simultaneously offer options and handle them,
    /// which has the benefit of ensuring at compile time that no case is left unhandled.
    ///
    /// This function returns the [`Receive::Error`] for the underlying `Rx` connection if there was
    /// an error while receiving.
    #[must_use]
    pub async fn offer(self) -> Result<Branches<Tx, Rx, (P, Ps), E>, Rx::Error> {
        let (tx, mut rx) = self.unwrap();
        match rx.recv().await {
            Ok(variant) if variant < <<(P, Ps) as AllSession>::Arity as Unary>::VALUE => {
                Ok(Branches {
                    variant,
                    tx,
                    rx,
                    protocols: PhantomData,
                    environment: PhantomData,
                })
            }
            Ok(variant) => {
                panic!(
                    "Out-of-range discriminant {} in `Chan::offer` (expected at most {}): \
                        this is always because the other party did not follow the dual of this \
                        session",
                    variant,
                    <<(P, Ps) as AllSession>::Arity as Unary>::VALUE - 1
                )
            }
            Err(err) => Err(err),
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! branches {
    (
        $branch:ident, $chan:ident, $code:expr, $($t:tt)+
    ) => (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => $code,
            std::result::Result::Err($branch) => $crate::branches!{ $branch, $chan, $($t)+ }
        }
    );
    (
        $branch:ident, $chan:ident, $code:expr $(,)?
    ) => (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => $code,
            std::result::Result::Err($branch) => $crate::Branches::empty_case($branch)
        }
    )
}

#[macro_export]
macro_rules! offer {
    (
        $chan:ident => $($t:tt)*
    ) => (
        {
            let b = $crate::Chan::offer($chan).await?;
            $crate::branches!{ b, $chan, $($t)* }
        }
    )
}

/// A placeholder for a missing transmit or receive end of a connection.
///
/// When using [`Chan::split`], the resultant two channels can only send or only receive,
/// respectively. This is reflected at the type level by the presence of `Unavailable` on the type
/// of the connection which is not present for each part of the split.
#[derive(Debug)]
pub struct Unavailable<T>(Arc<()>, PhantomData<T>);

impl<T> Clone for Unavailable<T> {
    fn clone(&self) -> Self {
        Unavailable(Arc::clone(&self.0), PhantomData)
    }
}

impl<T> Unavailable<T> {
    /// Make a new `Unavailable`.
    fn new() -> Self {
        Unavailable(Arc::new(()), PhantomData)
    }

    /// Cast an `Unavailable` to a new phantom type.
    fn cast<S>(self) -> Unavailable<S> {
        Unavailable(self.0, PhantomData)
    }
}

impl<Tx, Rx, P: Session, Q: Session, E> Chan<Tx, Rx, Split<P, Q>, E> {
    /// Split a channel into transmit-only and receive-only ends which may be used concurrently and
    /// reunited (provided they reach a matching session type) using [`Chan::unsplit`].
    #[must_use]
    pub fn split(
        self,
    ) -> (
        Chan<Tx, Unavailable<Rx>, P, E>,
        Chan<Unavailable<Tx>, Rx, Q, E>,
    ) {
        let (tx, rx) = self.unwrap();
        let unavailable = Unavailable::new();
        let tx_only = unsafe { Chan::with_env(tx, unavailable.clone()) };
        let rx_only = unsafe { Chan::with_env(unavailable.cast(), rx) };
        (tx_only, rx_only)
    }
}

impl<Tx, Rx, P: Session, E> Chan<Tx, Rx, P, E> {
    /// Reunite the transmit-only and receive-only channels resulting from a call to [`Chan::split`]
    /// into a single channel.
    ///
    /// # Panics
    ///
    /// If the two channels given as input did not result from the same call to [`Chan::split`].
    #[must_use]
    pub fn unsplit(
        mut tx: Chan<Tx, Unavailable<Rx>, P, E>,
        mut rx: Chan<Unavailable<Tx>, Rx, P, E>,
    ) -> Self {
        if Arc::ptr_eq(&tx.rx().0, &rx.tx().0) {
            unsafe { Chan::with_env(tx.unwrap().0, rx.unwrap().1) }
        } else {
            panic!("Unrelated channels passed to `Chan::unsplit`")
        }
    }
}

impl<Tx, Rx, E, P: Session> Chan<Tx, Rx, Loop<P>, E> {
    /// Enter a loop, permitting control to jump back to this point in the session using
    /// [`Chan::recur`].
    #[must_use]
    pub fn enter(self) -> Chan<Tx, Rx, P, (P, E)> {
        unsafe { self.cast() }
    }
}

impl<Tx, Rx, N: marker::Send + Any, E: Select<N>> Chan<Tx, Rx, Recur<N>, E>
where
    E::Selected: Session,
{
    /// Jump back to the beginning of a loop marked using [`Chan::enter`], resetting the session
    /// type of this channel to that at the beginning of the loop.
    #[must_use]
    pub fn recur(self) -> Chan<Tx, Rx, E::Selected, E> {
        unsafe { self.cast() }
    }
}

impl<Tx, Rx, P: Session, E> Chan<Tx, Rx, P, E> {
    /// Cast a channel to arbitrary new session types and environment. Use with care!
    #[must_use]
    unsafe fn cast<F, Q: Session>(self) -> Chan<Tx, Rx, Q, F> {
        let (tx, rx) = self.unwrap();
        Chan {
            tx: Some(tx),
            rx: Some(rx),
            environment: PhantomData,
            protocol: PhantomData,
        }
    }

    /// Unwrap a channel into its transmit and receive ends, exiting the regimen of session typing.
    fn unwrap(mut self) -> (Tx, Rx) {
        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        drop(std::mem::ManuallyDrop::new(self));
        (tx, rx)
    }

    /// Borrow the receiving channel (internal-use-only)
    fn rx(&mut self) -> &mut Rx {
        self.rx.as_mut().unwrap()
    }

    /// Borrow the transmitting channel (internal-use-only)
    fn tx(&mut self) -> &mut Tx {
        self.tx.as_mut().unwrap()
    }

    /// Create a new channel with an arbitrary environment. This is equivalent to casting a new
    /// channel to an arbitrary environment. Use with care!
    #[must_use]
    unsafe fn with_env(tx: Tx, rx: Rx) -> Chan<Tx, Rx, P, E> {
        Chan {
            tx: Some(tx),
            rx: Some(rx),
            environment: PhantomData,
            protocol: PhantomData,
        }
    }
}

mod sealed {
    use super::{Choose, End, Loop, Offer, Recur, Recv, Send, Split, S, Z};
    use std::{any::Any, marker};

    pub trait Session: marker::Send + Any {}
    impl Session for End {}
    impl<T: marker::Send + Any, P: Session> Session for Recv<T, P> {}
    impl<T: marker::Send + Any, P: Session> Session for Send<T, P> {}
    impl<Ps: AllSession> Session for Choose<Ps> {}
    impl<Ps: AllSession> Session for Offer<Ps> {}
    impl<P: Session, Q: Session> Session for Split<P, Q> {}
    impl<P: Session> Session for Loop<P> {}
    impl<N: marker::Send + Any> Session for Recur<N> {}

    pub trait AllSession: marker::Send + Any {}
    impl AllSession for () {}
    impl<T: Session, Ts: AllSession> AllSession for (T, Ts) {}

    pub trait Select<N> {}
    impl<T, S> Select<Z> for (T, S) {}
    impl<T, P, N> Select<S<N>> for (T, (P, ())) where (P, ()): Select<N> {}
}
