use async_trait::async_trait;
use std::{
    any::{Any, TypeId},
    future::Future,
    marker::{self, PhantomData},
    pin::Pin,
    sync::Arc,
};
use thiserror::Error;

pub use backend::*;
pub use types::*;

pub mod backend;
pub mod types;

/// A bidirectional communications channel sending outgoing messages via the `Tx` connection and
/// receiving incoming messages via the `Rx` connection, whose future session behavior is determined
/// by the session type `P`. The `E` parameter is a type-level list describing the *session
/// environment* of the channel: the stack of [`Loop`]s the channel has entered.
///
/// # Panics
///
/// For *unfinished* channels (that is, those with a session type that is not `End`), dropping them
/// before the end of their session will result in a panic.
///
/// # Examples
///
/// In this example, we make a one-message session type describing a session that sends a single
/// string and then ends. Then, we use a pair of channels with that session type and its dual to
/// enact the session:
///
/// ```
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Make a pair of channels:
/// // - `c1` with the session type `Send<String, End>`, and
/// // - `c2` with the dual session type `Recv<String, End>`
/// let (c1, c2) = <Send<String, End>>::channel(|| backend::mpsc::channel(1));
///
/// // Spawn a thread to send a message
/// let t1 = tokio::spawn(async move {
///     c1.send("Hello, world!".to_string()).await?;
///     Ok::<_, backend::mpsc::SendError<String>>(())
/// });
///
/// // Spawn a thread to receive a message
/// let t2 = tokio::spawn(async move {
///     let (s, c2) = c2.recv().await?;
///     assert_eq!(s, "Hello, world!");
///     Ok::<_, backend::mpsc::RecvError>(())
/// });
///
/// // Wait for both threads to complete
/// t1.await??;
/// t2.await??;
/// # Ok(())
/// # }
/// ```
///
/// However, suppose we accidentally tried to *send* on the channel we were supposed to *receive*
/// on. The following **does not compile**:
///
/// ```compile_fail
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Make a pair of channels:
/// // - `c1` with the session type `Send<String, End>`, and
/// // - `c2` with the dual session type `Recv<String, End>`
/// let (c1, c2) = <Send<String, End>>::channel(|| backend::mpsc::channel(1));
///
/// // try to send on `c2`, but type is `Recv<String, End>`
/// c2.send("Hello, world!".to_string()).await?;
///
/// // try to receive on `c1`, but type is `Send<String, End>`
/// let (s, c1) = c1.recv().await?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
#[must_use = "Dropping a channel before finishing its session type will result in a panic"]
pub struct Chan<Tx, Rx, P: Actionable<E>, E: Environment = ()>
where
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    tx: Option<Tx>, // never `None` unless currently in the body of `unwrap`
    rx: Option<Rx>, // never `None` unless currently in the body of `unwrap`
    environment: PhantomData<E>,
    protocol: PhantomData<P>,
}

/// The `NewSession` extension trait gives methods to create session-typed channels from session
/// types. These are implemented as static methods on the session type itself. For instance:
///
/// ```
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Send<String, End>>::channel(backend::mpsc::unbounded_channel);
/// // do something with these channels...
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
pub trait NewSession
where
    Self: Actionable<()>,
    Self::Dual: Actionable<()>,
    Self::Action: Actionable<Self::Env>,
    <Self::Dual as Actionable<()>>::Action: Actionable<<Self::Dual as Actionable<()>>::Env>,
    Self::Env: Environment,
    <Self::Dual as Actionable<()>>::Env: Environment,
    <Self::Env as EachSession>::Dual: Environment,
    <Self::Action as Actionable<Self::Env>>::Env: Environment,
    <<Self::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
    <<Self::Action as Actionable<Self::Env>>::Env as EachSession>::Dual: Environment,
    <<<Self::Dual as Actionable<()>>::Action as Actionable<<Self::Dual as Actionable<()>>::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Given a closure which generates a uni-directional underlying transport channel, create a
    /// pair of dual [`Chan`]s which communicate over the transport channels resulting from these
    /// closures.
    ///
    /// By internally wiring together the two directional channels, this function assures that
    /// communications over the channels actually follow the session specified.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = End::channel(backend::mpsc::unbounded_channel);
    /// # }
    /// ```
    #[must_use]
    fn channel<Tx, Rx>(
        make: impl FnMut() -> (Tx, Rx),
    ) -> (
        Chan<Tx, Rx, Self::Action, Self::Env>,
        Chan<Tx, Rx, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    );

    /// Given two closures, each of which generates a uni-directional underlying transport channel,
    /// create a pair of dual [`Chan`]s which communicate over the transport channels resulting from
    /// these closures.
    ///
    /// By internally wiring together the two directional channels, this function assures that
    /// communications over the channels actually follow the session specified.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = End::bichannel(
    ///     backend::mpsc::unbounded_channel,
    ///     || backend::mpsc::channel(1),
    /// );
    /// # }
    /// ```
    #[must_use]
    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self::Action, Self::Env>,
        Chan<Tx1, Rx0, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    );

    /// Given a transmitting and receiving end of an un-session-typed connection, wrap them in a new
    /// session-typed channel for the protocol `P.`
    ///
    /// It is expected that the other ends of these connections will be wrapped in a channel with
    /// the [`Dual`](crate::Session::Dual) session type.
    ///
    /// # Notes
    ///
    /// Because `&mut Tx` and `&mut Rx` are [`Transmit`] and [`Receive`] if `Tx` and `Rx` are
    /// [`Transmit`] and `Receive` respectively, a [`Chan`] does not need to own its connections; a
    /// mutable reference or an owned type work equally well as inputs to `wrap`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (mut tx, mut rx) = backend::mpsc::unbounded_channel();
    /// let c = End::wrap(&mut tx, &mut rx);  // you can wrap &mut references
    /// c.close();                            // whose lifetimes end when the channel is closed,
    /// let c = End::wrap(tx, rx);            // or you can wrap owned values
    /// let (tx, rx) = c.close();             // and get them back when the channel is closed.
    /// # }
    /// ```
    #[must_use]
    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self::Action, Self::Env>;
}

impl<P> NewSession for P
where
    Self: Actionable<()>,
    Self::Dual: Actionable<()>,
    Self::Action: Actionable<Self::Env>,
    <Self::Dual as Actionable<()>>::Action: Actionable<<Self::Dual as Actionable<()>>::Env>,
    Self::Env: Environment,
    <Self::Dual as Actionable<()>>::Env: Environment,
    <Self::Env as EachSession>::Dual: Environment,
    <Self::Action as Actionable<Self::Env>>::Env: Environment,
    <<Self::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
    <<Self::Action as Actionable<Self::Env>>::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable<()>>::Action as Actionable<
        <Self::Dual as Actionable<()>>::Env,
    >>::Env: Environment,
    <<<Self::Dual as Actionable<()>>::Action as Actionable<
        <Self::Dual as Actionable<()>>::Env,
    >>::Env as EachSession>::Dual: Environment,
{
    #[must_use]
    fn channel<Tx, Rx>(
        mut make: impl FnMut() -> (Tx, Rx),
    ) -> (
        Chan<Tx, Rx, Self::Action, Self::Env>,
        Chan<Tx, Rx, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    ) {
        let (tx0, rx0) = make();
        let (tx1, rx1) = make();
        (P::wrap(tx0, rx1), <P::Dual>::wrap(tx1, rx0))
    }

    #[must_use]
    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self::Action, Self::Env>,
        Chan<Tx1, Rx0, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    ) {
        let (tx0, rx0) = make0();
        let (tx1, rx1) = make1();
        (P::wrap(tx0, rx1), <P::Dual>::wrap(tx1, rx0))
    }

    #[must_use]
    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self::Action, Self::Env> {
        unsafe { Chan::with_env(tx, rx) }
    }
}

impl<Tx, Rx, P, E> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = End>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Close a finished session, returning the wrapped connections used during the session.
    ///
    /// This function does not do cleanup on the actual underlying connections; they are passed back
    /// to the caller who may either continue to use them outside the session typing context or
    /// clean them up as appropriate.
    ///
    /// # Examples
    ///
    /// Starting with a channel whose session type is already `End`, we can immediately close the
    /// channel.
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = End::channel(backend::mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.close();
    /// let (tx2, rx2) = c2.close();
    /// # }
    /// ```
    ///
    /// However, if the channel's session type is *not* `End`, it is a type error to attempt to
    /// close the channel and retrieve its underlying sender and receiver. The following code **will
    /// not compile**:
    ///
    /// ```compile_fail
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = <Loop<Send<String, Recur>>>::channel(backend::mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.close();
    /// let (tx2, rx2) = c2.close();
    /// # }
    /// ```
    ///
    /// Note too: dropping such a non-`End` channel will result in a panic. If you *really* want to
    /// destruct a channel before the end of its session, use [`Chan::unwrap`], but beware that this
    /// may cause the party on the other end of the channel to throw errors due to your violation of
    /// the channel's protocol!
    pub fn close(self) -> (Tx, Rx) {
        self.unwrap()
    }
}

// This `Drop` implementation panics if the session isn't over.
impl<Tx, Rx, P, E> Drop for Chan<Tx, Rx, P, E>
where
    P: Actionable<E>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    fn drop(&mut self) {
        let mut this = std::mem::ManuallyDrop::new(self);
        drop(this.rx.take());
        drop(this.tx.take());
        if TypeId::of::<P>() != TypeId::of::<End>() {
            panic!("Channel dropped before finishing its session")
        }
    }
}

impl<Tx, Rx, E, P, T, Q> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Recv<T, Q>> + 'static,
    <P::Env as EachSession>::Dual: Environment,
    Q: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    T: marker::Send + 'static,
    Rx: Receive<T>,
    Q::Action: Actionable<<Q as Actionable<P::Env>>::Env>,
    Q::Env: Environment,
    <Q::Env as EachSession>::Dual: Environment,
    <<Q::Action as Actionable<Q::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Receive something of type `T` on the channel, returning the pair of the received object and
    /// the channel.
    ///
    /// # Errors
    ///
    /// This function returns the [`Receive::Error`] for the underlying `Rx` connection if there was
    /// an error while receiving.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Recv<String, End>>::channel(|| backend::mpsc::channel(1));
    /// c2.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c1) = c1.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn recv(mut self) -> Result<(T, Chan<Tx, Rx, Q::Action, Q::Env>), Rx::Error> {
        match self.rx().recv().await {
            Ok(result) => Ok((result, unsafe { self.cast() })),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

impl<'a, Tx, Rx, E, P: 'static, T: 'static, Q> Chan<Tx, Rx, P, E>
where
    E: Environment,
    E::Dual: Environment,
    P: Actionable<E, Action = Send<T, Q>>,
    <P::Env as EachSession>::Dual: Environment,
    Q: Actionable<P::Env>,
    E: EachSession,
    Q::Action: Actionable<<Q as Actionable<P::Env>>::Env>,
    Q::Env: Environment,
    <Q::Env as EachSession>::Dual: Environment,
    <<Q::Action as Actionable<Q::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Send something of type `T` on the channel, returning the channel.
    ///
    /// The underlying sending channel `Tx` may be able to send a `T` using multiple different
    /// [`CallingConvention`]s: by [`Val`], by [`Ref`] and/or by [`Mut`]. To disambiguate, use
    /// "turbofish" syntax when calling `send`, i.e. `chan.send::<Val>(1)` or
    /// `chan.send::<Ref>(&true)`.
    ///
    /// # Errors
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Send<String, End>>::channel(|| backend::mpsc::channel(1));
    /// c1.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c2) = c2.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn send<Convention: CallingConvention>(
        mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Result<Chan<Tx, Rx, Q::Action, Q::Env>, <Tx as Transmit<'a, T, Convention>>::Error>
    where
        Tx: Transmit<'a, T, Convention>,
        T: CallBy<'a, Convention>,
        <T as CallBy<'a, Convention>>::Type: marker::Send,
    {
        match self.tx().send(message).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

impl<'a, Tx, Rx, E, P, Choices> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Choose<Choices>>,
    <P::Env as EachSession>::Dual: Environment,
    Tx: Transmit<'static, usize, Val>,
    E: Environment,
    E::Dual: Environment,
    Choices: EachActionable<P::Env>,
    P::Env: Environment,
{
    /// Actively choose to enter the `N`th protocol offered via [`Chan::offer`] by the other end of
    /// the connection, alerting the other party to this choice by sending the number `N` over the
    /// channel.
    ///
    /// # Errors
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending the choice.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64, End>, (Recv<String, End>, ()))>;
    ///
    /// let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
    ///
    /// // Spawn a thread to offer a choice
    /// let t1 = tokio::spawn(async move {
    ///     offer! { c2 =>
    ///         { c2.recv().await?; },
    ///         { c2.send("Hello!".to_string()).await?; },
    ///     }
    ///     Ok::<_, backend::mpsc::Error>(())
    /// });
    ///
    /// // Choose to send an integer
    /// c1.choose::<Z>().await?.send(42).await?;
    ///
    /// // Wait for the offering thread to finish
    /// t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn choose<N: Unary>(
        mut self,
    ) -> Result<
        Chan<
            Tx,
            Rx,
            <Choices::Selected as Actionable<E>>::Action,
            <Choices::Selected as Actionable<E>>::Env,
        >,
        Tx::Error,
    >
    where
        Choices: Select<N>,
        Choices::Selected: Actionable<E>,
        <Choices::Selected as Actionable<E>>::Env: Environment,
        <<Choices::Selected as Actionable<E>>::Env as EachSession>::Dual: Environment,
        <Choices::Selected as Actionable<E>>::Action:
            Actionable<<Choices::Selected as Actionable<E>>::Env>,
        <<<Choices::Selected as Actionable<E>>::Action as Actionable<
            <Choices::Selected as Actionable<E>>::Env,
        >>::Env as EachSession>::Dual: Environment,
    {
        match self.tx().send(N::VALUE).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

/// The result of [`Chan::offer`], `Branches<Tx, Rx, Ps>` represents an enumeration of the possible
/// new channel states that could result from the offering of the type-level list of protocols `Ps`.
///
/// To find out which protocol was selected by the other party, use [`Branches::case`], or better
/// yet, use the [`offer!`](crate::offer) macro to ensure you don't miss any cases.
#[derive(Debug)]
#[must_use]
pub struct Branches<Tx, Rx, Ps: EachActionable<E>, E: Environment = ()>
where
    E::Dual: Environment,
{
    variant: usize,
    tx: Option<Tx>, // never `None` unless in the body of `case` or `drop`
    rx: Option<Rx>, // never `None` unless in the body of `case` or `drop`
    protocols: PhantomData<Ps>,
    environment: PhantomData<E>,
}

impl<Tx, Rx, Ps: EachActionable<E>, E: Environment> Drop for Branches<Tx, Rx, Ps, E>
where
    E::Dual: Environment,
{
    fn drop(&mut self) {
        let mut this = std::mem::ManuallyDrop::new(self);
        drop(this.rx.take());
        drop(this.tx.take());
        if TypeId::of::<Ps>() != TypeId::of::<()>() {
            panic!("`Branches` of offered protocols dropped without matching all cases (use `Branches::case` and `Branches::empty_case` or the `offer!` macro to eliminate branches)")
        }
    }
}

impl<'a, Tx, Rx, P, Ps, E> Branches<Tx, Rx, (P, Ps), E>
where
    Ps: EachActionable<E>,
    P: Actionable<E>,
    P::Dual: Actionable<E::Dual>,
    <<P::Dual as Actionable<E::Dual>>::Env as EachSession>::Dual: Environment,
    P::Env: Environment,
    <P::Env as EachSession>::Dual: Environment,
    P::Action: Actionable<P::Env>,
    <<P::Action as Actionable<P::Env>>::Env as EachSession>::Dual: Environment,
    E: Environment,
    E::Dual: Environment,
{
    /// Check if the selected protocol in this [`Branches`] was `P`. If so, return the corresponding
    /// channel; otherwise, return all the other possibilities.
    #[must_use = "all possible choices must be handled (add cases to match the type of this `Offer<...>`)"]
    pub fn case(
        mut self,
    ) -> Result<
        Chan<Tx, Rx, <P as Actionable<E>>::Action, <P as Actionable<E>>::Env>,
        Branches<Tx, Rx, Ps, E>,
    > {
        let variant = self.variant;
        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        let _ = std::mem::ManuallyDrop::new(self);
        if variant == 0 {
            Ok(unsafe { Chan::with_env(tx, rx) })
        } else {
            Err(Branches {
                variant: variant - 1,
                tx: Some(tx),
                rx: Some(rx),
                protocols: PhantomData,
                environment: PhantomData,
            })
        }
    }
}

/// An error representing a protocol violation where the other end of the channel has selected a
/// choice whose index is too high to be handled by the current session type.
///
/// For instance, if the current channel's session type is `Offer<(..., (..., (..., ())))>`, then
/// this error is thrown if an invocation of `offer!` encounters a discriminant sent from the other
/// side which is greater than `2`.
///
/// Authors of backends may wish to write a `From<OutOfRangeChoice>` implementation for their
/// backend's error type so that these errors are automatically handled.
#[derive(Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[error("received discriminant out of range in `Chan::offer`")]
pub struct OutOfRangeChoice;

impl<'a, Tx, Rx, E: Environment> Branches<Tx, Rx, (), E>
where
    E::Dual: Environment,
{
    /// Attempt to eliminate an empty [`Branches`], returning an error if the originating
    /// discriminant for this set of protocol choices was out of range.
    ///
    /// This function is only callable on empty [`Branches`], which under ordinary circumstances
    /// means it proves the unreachability of its calling location. However, if the other end of the
    /// channel breaks protocol, an empty [`Branches`] can in fact be constructed, and this function
    /// will then signal an error.
    pub fn empty_case<T>(self) -> Result<T, OutOfRangeChoice> {
        Err(OutOfRangeChoice)
    }
}

impl<'a, Tx, Rx, P, Q, Qs, E> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Offer<(Q, Qs)>>,
    P::Env: EachSession,
    Q::Dual: Actionable<E::Dual>,
    <<Q::Dual as Actionable<E::Dual>>::Env as EachSession>::Dual: Environment,
    Q: Actionable<P::Env>,
    <<Q as Actionable<P::Env>>::Env as EachSession>::Dual: Environment,
    Q::Dual: Actionable<<P::Env as EachSession>::Dual>,
    <<Q::Dual as Actionable<<P::Env as EachSession>::Dual>>::Env as EachSession>::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    Qs: EachActionable<P::Env>,
    P::Env: Environment,
    E::Dual: Environment,
    Rx: Receive<usize>,
    E: Environment,
{
    /// Offer the choice of one or more protocols to the other party, and wait for them to indicate
    /// by sending a number which protocol to proceed with.
    ///
    /// # Notes
    ///
    /// Where possible, prefer the [`offer!`](crate::offer) macro to simultaneously offer options
    /// and handle them. This has the benefit of ensuring at compile time that no case is left
    /// unhandled; it's also more succinct.
    ///
    /// # Errors
    ///
    /// This function returns the [`Receive::Error`] for the underlying `Rx` connection if there was
    /// an error while receiving.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64, End>, (Recv<String, End>, ()))>;
    ///
    /// let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
    ///
    /// // Spawn a thread to offer a choice
    /// let t1 = tokio::spawn(async move {
    ///     match c2.offer().await?.case() {
    ///         Ok(c2) => { c2.recv().await?; },
    ///         Err(rest) => match rest.case() {
    ///             Ok(c2) => { c2.send("Hello!".to_string()).await?; },
    ///             Err(rest) => rest.empty_case()?,
    ///         }
    ///     }
    ///     Ok::<_, backend::mpsc::Error>(())
    /// });
    ///
    /// // Choose to send an integer
    /// c1.choose::<Z>().await?.send(42).await?;
    ///
    /// // Wait for the offering thread to finish
    /// t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Notice how the handling of cases by manual `match` is harder to read than the equivalent in
    /// terms of [`offer!`](crate::offer):
    ///
    /// ```
    /// # use dialectic::*;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type GiveOrTake = Choose<(Send<i64, End>, (Recv<String, End>, ()))>;
    /// #
    /// # let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
    /// #
    /// # // Spawn a thread to offer a choice
    /// # let t1 = tokio::spawn(async move {
    /// offer! { c2 =>
    ///     { c2.recv().await?; },
    ///     { c2.send("Hello!".to_string()).await?; },
    /// }
    /// # Ok::<_, backend::mpsc::Error>(())
    /// # });
    /// #
    /// # // Choose to send an integer
    /// # c1.choose::<Z>().await?.send(42).await?;
    /// #
    /// # // Wait for the offering thread to finish
    /// # t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn offer(self) -> Result<Branches<Tx, Rx, (Q, Qs), P::Env>, Rx::Error> {
        let (tx, mut rx) = self.unwrap();
        match rx.recv().await {
            Ok(variant) => Ok(Branches {
                variant,
                tx: Some(tx),
                rx: Some(rx),
                protocols: PhantomData,
                environment: PhantomData,
            }),
            Err(err) => Err(err),
        }
    }
}

/// Offer a set of different protocols, allowing the other side of the channel to choose with which
/// one to proceed. This macro only works in a `Try` context, i.e. somewhere the `?` operator would
/// make sense to use.
///
/// # Notes
///
/// - You must specify exactly as many branches as there are options in the type of the
///   [`Offer`](crate::types::Offer) to which this expression corresponds.
/// - In the body of each branch, the identifier for the channel is rebound to have the session type
///   corresponding to that branch.
/// - To use `offer!` as an expression, ensure the type of every branch matches.
///
/// # Examples
///
/// ```
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// type GiveOrTake = Choose<(Send<i64, End>, (Recv<String, End>, ()))>;
///
/// let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
///
/// // Spawn a thread to offer a choice
/// let t1 = tokio::spawn(async move {
///     offer! { c2 =>
///         { c2.recv().await?; },
///         { c2.send("Hello!".to_string()).await?; },
///     }
///     Ok::<_, backend::mpsc::Error>(())
/// });
///
/// // Choose to send an integer
/// c1.choose::<Z>().await?.send(42).await?;
///
/// // Wait for the offering thread to finish
/// t1.await??;
/// # Ok(())
/// # }
/// ```
///
/// # Errors
///
/// The generated code may result in the [`Transmit::Error`] of the underlying connection, as well
/// as [`OutOfRangeChoice`] in the case where the other side of the connection breaks the protocol
/// and selects a choice that does not exist. If the other side of the channel is also using this
/// session type, that case is impossible.
///
/// Alternatively, you may specify an optional clause to handle protocol violations where the other
/// side signals an invalid choice. In the example above, we could choose to immediately panic in
/// that scenario:
///
/// ```
/// # use dialectic::*;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # type GiveOrTake = Choose<(Send<i64, End>, (Recv<String, End>, ()))>;
/// #
/// # let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
/// #
/// # // Spawn a thread to offer a choice
/// # let t1 = tokio::spawn(async move {
/// offer! { c2 =>
///     { c2.recv().await?; },
///     { c2.send("Hello!".to_string()).await?; },
///     ? => panic!("Protocol violation!"),
/// }
/// # Ok::<_, backend::mpsc::Error>(())
/// # });
/// #
/// # // Choose to send an integer
/// # c1.choose::<Z>().await?.send(42).await?;
/// #
/// # // Wait for the offering thread to finish
/// # t1.await??;
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! offer {
    (
        $chan:ident => $($t:tt)*
    ) => (
        {
            match $crate::Chan::offer($chan).await {
                Ok(b) => $crate::offer!{@branches b, $chan, $($t)* },
                Err(e) => Err(e)?,
            }
        }
    );
    (
        @branches $branch:ident, $chan:ident, $code:expr $(,)?
    ) =>
    (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => $code,
            std::result::Result::Err($branch) => $crate::Branches::empty_case($branch)?,
        }
    );
    (
        @branches $branch:ident, $chan:ident, $code:expr, $($t:tt)+
    ) => (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => $code,
            std::result::Result::Err($branch) => $crate::offer!{@branches $branch, $chan, $($t)+ },
        }
    );
    (
        @branches $branch:ident, $chan:ident, ? => $empty:expr $(,)?
    ) => (
        {
            let _: $crate::Branches<_, _, (), _> = $branch;
            $empty
        }
    );
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

// Empty catch-all impl that prevents anyone from actually instantiating `Transmit` for
// `Unavailable`. Since there are no impls of `Unsatisfiable`, this can never be invoked.
impl<'a, C, T, Convention> Transmit<'a, T, Convention> for Unavailable<C>
where
    Self: sealed::Unsatisfiable,
    T: CallBy<'a, Convention>,
    Convention: CallingConvention,
    <T as CallBy<'a, Convention>>::Type: marker::Send,
{
    type Error = ();
    type Future = Pin<Box<dyn Future<Output = Result<(), ()>> + marker::Send>>;

    fn send(&mut self, _: <T as CallBy<'a, Convention>>::Type) -> Self::Future {
        unreachable!("Impossible to send on `Unavailable` sender")
    }
}

// Empty catch-all impl that prevents anyone from actually instantiating `Receive` for
// `Unavailable`. Since there are no impls of `Unsatisfiable`, this can never be invoked.
#[async_trait]
impl<C: marker::Send, T: 'static> Receive<T> for Unavailable<C>
where
    Self: sealed::Unsatisfiable,
{
    type Error = ();

    async fn recv(&mut self) -> Result<T, Self::Error> {
        unreachable!("Impossible to receive on `Unavailable` receiver")
    }
}

impl<Tx, Rx, P, E> Chan<Tx, Rx, P, E>
where
    P: Actionable<E>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Split a channel into transmit-only and receive-only ends which may be used concurrently and
    /// reunited (provided they reach a matching session type) using [`Chan::unsplit`].
    #[must_use]
    pub fn split<Q, R>(
        self,
    ) -> (
        Chan<Tx, Unavailable<Rx>, Q::Action, Q::Env>,
        Chan<Unavailable<Tx>, Rx, R::Action, R::Env>,
    )
    where
        P: Actionable<E, Action = Split<Q, R>>,
        Q: Actionable<P::Env>,
        R: Actionable<P::Env>,
        Q::Action: Actionable<Q::Env>,
        R::Action: Actionable<R::Env>,
        <P::Env as EachSession>::Dual: Environment,
        <Q::Env as EachSession>::Dual: Environment,
        <R::Env as EachSession>::Dual: Environment,
        <<<Q as Actionable<P::Env>>::Action as Actionable<<Q as Actionable<P::Env>>::Env>>::Env as EachSession>::Dual: Environment,
        <<<R as Actionable<P::Env>>::Action as Actionable<<R as Actionable<P::Env>>::Env>>::Env as EachSession>::Dual: Environment,
    {
        let (tx, rx) = self.unwrap();
        let unavailable = Unavailable::new();
        let tx_only = unsafe { Chan::with_env(tx, unavailable.clone()) };
        let rx_only = unsafe { Chan::with_env(unavailable.cast(), rx) };
        (tx_only, rx_only)
    }
}

impl<Tx, Rx, P, E> Chan<Tx, Rx, P, E>
where
    P: Actionable<E>,
    <P::Env as EachSession>::Dual: Environment,
    E: Environment,
    E::Dual: Environment,
{
    /// Reunite the transmit-only and receive-only channels resulting from a call to [`Chan::split`]
    /// into a single channel.
    ///
    /// # Panics
    ///
    /// If the two channels given as input did not result from the same call to [`Chan::split`].
    #[must_use]
    pub fn unsplit<Q, R>(
        mut tx: Chan<Tx, Unavailable<Rx>, Q, E>,
        mut rx: Chan<Unavailable<Tx>, Rx, R, E>,
    ) -> Self
    where
        Q: Actionable<E, Action = P, Env = E>,
        R: Actionable<E, Action = P, Env = E>,
    {
        if Arc::ptr_eq(&tx.rx().0, &rx.tx().0) {
            unsafe { Chan::with_env(tx.unwrap().0, rx.unwrap().1) }
        } else {
            panic!("Unrelated channels passed to `Chan::unsplit`")
        }
    }
}

/// Enter a [`Loop`](crate::types::Loop) on a channel and repeatedly execute the given block until a
/// `break` statement is encountered. This is syntactic sugar over `loop`.
///
/// # Notes
///
/// The expression specified as the body of the loop must return the final value of the channel at
/// the end of the loop iteration.
///
/// If the channel is to be used again after the loop is exited, it's necessary to break with a
/// return value of the channel; otherwise the channel will be dropped at the end of the loop.
#[macro_export]
macro_rules! loop_ {
    ($($label:lifetime :)? $chan:ident => $($t:tt)*) => {
        {
            let mut $chan = $chan;
            $($label :)? loop {
                let c = { $($t)* };
                $chan = c;
            }
        }
    };
}

/// Iterate over some iterable collection, performing one [`Loop`](crate::types::Loop) on the
/// channel for each item in the collection, and returning the channel as output. This is syntactic
/// sugar over `for`.
///
/// # Notes
///
/// The expression specified as the body of the for-loop must return the final value of the channel
/// at the end of the loop iteration.
#[macro_export]
macro_rules! for_ {
    ($($label:lifetime :)? $x:pat in $e:expr, $chan:ident => $($t:tt)*) => {
        {
            let mut $chan = $chan;
            $($label :)? for $x in $e {
                let c = { $($t)* };
                $chan = c;
            }
            $chan
        }
    }
}

/// Enter a [`Loop`](crate::types::Loop) on a channel and repeatedly execute the given block until a
/// condition evaluates to false (or a pattern fails to match). This is syntactic sugar over `while`
/// and `while let`.
///
/// # Notes
///
/// The expression specified as the body of the loop must return the final value of the channel at
/// the end of the loop iteration.
#[macro_export]
macro_rules! while_ {
    ($($label:lifetime :)? $cond:expr, $chan:ident => $($t:tt)*) => {
        {
            let mut $chan = $chan;
            $($label :)? while $cond {
                let c = { $($t)* };
                $chan = c;
            }
            $chan
        }
    };
    ($($label:lifetime :)? let $p:pat = $e:expr, $chan:ident => $($t:tt)*) => {
        {
            let mut $chan = $chan;
            $($label :)? while let $p = $e {
                let c = { $($t)* };
                $chan = c;
            }
            $chan
        }
    }
}

impl<Tx, Rx, P: Any, E> Chan<Tx, Rx, P, E>
where
    E: Environment,
    E::Dual: Environment,
    P: Actionable<E>,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Cast a channel to arbitrary new session types and environment. Use with care!
    #[must_use]
    unsafe fn cast<F, Q>(self) -> Chan<Tx, Rx, Q, F>
    where
        F: Environment,
        F::Dual: Environment,
        Q: Actionable<F>,
        <Q::Env as EachSession>::Dual: Environment,
    {
        let (tx, rx) = self.unwrap();
        Chan {
            tx: Some(tx),
            rx: Some(rx),
            environment: PhantomData,
            protocol: PhantomData,
        }
    }

    /// Unwrap a channel into its transmit and receive ends, exiting the regimen of session typing,
    /// potentially before the end of the session.
    ///
    /// # Errors
    ///
    /// If this function is used before the end of a session, it may result in errors when the other
    /// end of the channel attempts to continue the session.
    pub fn unwrap(mut self) -> (Tx, Rx) {
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
    use super::*;
    use std::any::Any;

    pub trait IsSession: Any {}
    impl IsSession for End {}
    impl<T: Any, P: IsSession> IsSession for Recv<T, P> {}
    impl<T: Any, P: IsSession> IsSession for Send<T, P> {}
    impl<Ps: EachSession> IsSession for Choose<Ps> {}
    impl<Ps: EachSession> IsSession for Offer<Ps> {}
    impl<P: IsSession, Q: IsSession> IsSession for Split<P, Q> {}
    impl<P: IsSession> IsSession for Loop<P> {}
    impl<N: Any> IsSession for Recur<N> {}

    pub trait EachSession: Any {}
    impl EachSession for () {}
    impl<T: IsSession, Ts: EachSession> EachSession for (T, Ts) {}

    pub trait Select<N> {}
    impl<T, S> Select<Z> for (T, S) {}
    impl<T, P, N> Select<S<N>> for (T, (P, ())) where (P, ()): Select<N> {}

    pub trait Unsatisfiable {}
}
