use std::{
    marker::{self, PhantomData},
    sync::Arc,
};
use thiserror::Error;

use crate::backend::*;
use tuple::{List, Tuple};
pub use types::*;

pub mod backend;
pub mod types;

/// A bidirectional communications channel sending outgoing messages via the `Tx` connection and
/// receiving incoming messages via the `Rx` connection, whose future session behavior is determined
/// by the session type `P`. The `E` parameter is a type-level list describing the *session
/// environment* of the channel: the stack of [`Loop`]s the channel has entered.
///
/// # Examples
///
/// To construct a new [`Chan`], use one of the static methods of [`NewSession`] on the session type
/// for which you want to create a channel. Here, we use the session type `Send<String>`:
///
/// ```
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Make a pair of channels:
/// // - `c1` with the session type `Send<String>`, and
/// // - `c2` with the dual session type `Recv<String>`
/// let (c1, c2) = <Send<String>>::channel(|| backend::mpsc::channel(1));
/// # Ok(())
/// # }
/// ```
///
/// Then, you can use a pair of channels with that session type and its dual to enact the session:
///
/// ```
/// # use dialectic::*;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # let (c1, c2) = <Send<String>>::channel(|| backend::mpsc::channel(1));
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
/// # use dialectic::*;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # // Make a pair of channels:
/// # // - `c1` with the session type `Send<String>`, and
/// # // - `c2` with the dual session type `Recv<String>`
/// let (c1, c2) = <Send<String>>::channel(|| backend::mpsc::channel(1));
///
/// // Try to send on `c2`, but type is `Recv<String>`
/// c2.send("Hello, world!".to_string()).await?;
///
/// // Try to receive on `c1`, but type is `Send<String>`
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
    tx: Tx,
    rx: Rx,
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
/// let (c1, c2) = <Send<String>>::channel(backend::mpsc::unbounded_channel);
/// // do something with these channels...
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
///
/// # Notes
///
/// The trait bounds specified for [`NewSession`] ensure that the session type is well-formed.
/// However, it does not ensure that all the types in the session type can be sent or received over
/// the given channels.
///
/// Valid session types must contain only "productive" recursion: they must not contain any
/// [`Recur`] directly inside the loop to which that recursion refers. For instance, attempting to
/// use the session type `Loop<Recur>` will result in a compile-time trait solver overflow.
///
/// ```compile_fail
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Loop<Recur>>::channel(backend::mpsc::unbounded_channel);
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
///
/// This results in the compiler error:
///
///
/// ```text
/// error[E0275]: overflow evaluating the requirement `Recur: Actionable<(Recur, ())>`
///   |
/// 7 | let (c1, c2) = <Loop<Recur>>::channel(backend::mpsc::unbounded_channel);
///   |                ^^^^^^^^^^^^^^^^^^^^^^
///   |
///   = help: consider adding a `#![recursion_limit="256"]` attribute to your crate
///   = note: required because of the requirements on the impl of `Actionable<()>` for `Loop<Recur>`
///   = note: required because of the requirements on the impl of `NewSession` for `Loop<Recur>`
/// ```
///
/// In this situation, you **should not** take `rustc`'s advice. If you add a
/// `#![recursion_limit="256"]` attribute to your crate: this will only cause the compiler to work
/// harder before giving you the same error!
///
/// What the compiler is trying to tell you is that the session type as specified does not
/// correspond to a valid sequence of actions on the channel, because it contains unproductive
/// recursion.
pub trait NewSession
where
    Self: Actionable<()>,
    Self::Dual: Actionable<()>,
    <Self::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
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
    <Self::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
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
    /// If you *really* want to destruct a channel before the end of its session, use
    /// [`Chan::unwrap`], but beware that this may cause the party on the other end of the channel
    /// to throw errors due to your violation of the channel's protocol!
    pub fn close(self) -> (Tx, Rx) {
        self.unwrap()
    }
}

impl<Tx, Rx, E, P, T, Q> Chan<Tx, Rx, P, E>
where
    Rx: Receive<T>,
    T: marker::Send + 'static,
    P: Actionable<E, Action = Recv<T, Q>>,
    Q: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
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
    /// let (c1, c2) = <Recv<String>>::channel(|| backend::mpsc::channel(1));
    /// c2.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c1) = c1.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn recv(mut self) -> Result<(T, Chan<Tx, Rx, Q::Action, Q::Env>), Rx::Error> {
        match self.rx.recv().await {
            Ok(result) => Ok((result, unsafe { self.cast() })),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

impl<'a, Tx, Rx, E, P, T, Q> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Send<T, Q>>,
    Q: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
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
    /// let (c1, c2) = <Send<String>>::channel(|| backend::mpsc::channel(1));
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
        match self.tx.send(message).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => {
                drop(self.unwrap()); // drop without panicking
                Err(err)
            }
        }
    }
}

impl<Tx, Rx, E, P, Choices> Chan<Tx, Rx, P, E>
where
    Tx: Transmit<'static, u8, Val>,
    P: Actionable<E, Action = Choose<Choices>>,
    Choices: Tuple,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    Choices::AsList: EachScoped<<P::Env as Environment>::Depth>,
{
    /// Actively choose to enter the `N`th protocol offered via [`Chan::offer`] by the other end of
    /// the connection, alerting the other party to this choice by sending the number `N` over the
    /// channel.
    ///
    /// The choice `N` is specified as a type-level [`Unary`] number, i.e. `S(S(...Z...))`. For
    /// small `N`, predefined constants are available in the [`constants`] module, named for its
    /// corresponding decimal number prefixed with an underscore (e.g. `_0`, or `_42`).
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
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
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
    /// c1.choose(_0).await?.send(42).await?;
    ///
    /// // Wait for the offering thread to finish
    /// t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Attempting to choose an index that's out of bounds results in a compile-time error:
    ///
    /// ```compile_fail
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type OnlyTwoChoices = Choose<(End, End)>;
    /// let (c1, c2) = OnlyTwoChoices::channel(|| backend::mpsc::channel(1));
    ///
    /// // Try to choose something out of range (this doesn't typecheck)
    /// c1.choose(_2).await?;
    ///
    /// # // Wait for the offering thread to finish
    /// # t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn choose<N: Unary>(
        mut self,
        _choice: N,
    ) -> Result<
        Chan<
            Tx,
            Rx,
            <<Choices::AsList as Select<N>>::Selected as Actionable<E>>::Action,
            <<Choices::AsList as Select<N>>::Selected as Actionable<E>>::Env,
        >,
        Tx::Error,
    >
    where
        N: LessThan<unary::types::_128>,
        Choices::AsList: Select<N>,
        <Choices::AsList as Select<N>>::Selected: Actionable<E>,
        <<<Choices::AsList as Select<N>>::Selected as Actionable<E>>::Env as EachSession>::Dual:
            Environment,
    {
        match self.tx.send(N::VALUE as u8).await {
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
pub struct Branches<Tx, Rx, Choices, E = ()>
where
    Choices: Tuple,
    Choices::AsList: EachActionable<E>,
    E: Environment,
    E::Dual: Environment,
{
    variant: u8,
    tx: Tx,
    rx: Rx,
    protocols: PhantomData<Choices>,
    environment: PhantomData<E>,
}

impl<'a, Tx, Rx, Choices, P, Ps, E> Branches<Tx, Rx, Choices, E>
where
    Choices: Tuple<AsList = (P, Ps)>,
    (P, Ps): List<AsTuple = Choices>,
    P: Actionable<E>,
    Ps: EachActionable<E> + List,
    E: Environment,
    E::Dual: Environment,
    P::Dual: Actionable<E::Dual>,
    <P::Env as EachSession>::Dual: Environment,
    <<P::Action as Actionable<P::Env>>::Env as EachSession>::Dual: Environment,
    <<P::Dual as Actionable<E::Dual>>::Env as EachSession>::Dual: Environment,
{
    /// Check if the selected protocol in this [`Branches`] was `P`. If so, return the corresponding
    /// channel; otherwise, return all the other possibilities.
    #[must_use = "all possible choices must be handled (add cases to match the type of this `Offer<...>`)"]
    pub fn case(
        self,
    ) -> Result<
        Chan<Tx, Rx, <P as Actionable<E>>::Action, <P as Actionable<E>>::Env>,
        Branches<Tx, Rx, Ps::AsTuple, E>,
    > {
        let variant = self.variant;
        let tx = self.tx;
        let rx = self.rx;
        if variant == 0 {
            Ok(unsafe { Chan::with_env(tx, rx) })
        } else {
            Err(Branches {
                variant: variant - 1,
                tx,
                rx,
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

impl<'a, Tx, Rx, E, P, Choices> Chan<Tx, Rx, P, E>
where
    Rx: Receive<u8>,
    P: Actionable<E, Action = Offer<Choices>>,
    Choices: Tuple,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    Choices::AsList: EachActionable<P::Env>,
    Choices::AsList: EachScoped<<P::Env as Environment>::Depth>,
{
    /// Offer the choice of one or more protocols to the other party, and wait for them to indicate
    /// by sending a number which protocol to proceed with.
    ///
    /// # Notes
    ///
    /// **Where possible, prefer the [`offer!`](crate::offer) macro**. This has the benefit of
    /// ensuring at compile time that no case is left unhandled; it's also more succinct.
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
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
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
    /// c1.choose(_0).await?.send(42).await?;
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
    /// # use dialectic::constants::*;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
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
    /// # c1.choose(_0).await?.send(42).await?;
    /// #
    /// # // Wait for the offering thread to finish
    /// # t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub async fn offer(self) -> Result<Branches<Tx, Rx, Choices, P::Env>, Rx::Error> {
        let (tx, mut rx) = self.unwrap();
        match rx.recv().await {
            Ok(variant) => Ok(Branches {
                variant,
                tx: tx,
                rx: rx,
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
/// use dialectic::constants::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
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
/// c1.choose(_0).await?.send(42).await?;
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
/// # use dialectic::constants::*;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
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
/// # c1.choose(_0).await?.send(42).await?;
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

impl<Tx, Rx, E, P, Q, R> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Split<Q, R>>,
    Q: Actionable<P::Env>,
    R: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <<<Q as Actionable<P::Env>>::Action as Actionable<<Q as Actionable<P::Env>>::Env>>::Env as EachSession>::Dual: Environment,
    <<<R as Actionable<P::Env>>::Action as Actionable<<R as Actionable<P::Env>>::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Split a channel into transmit-only and receive-only ends which may be used concurrently and
    /// reunited (provided they reach a matching session type) using [`Chan::unsplit`].
    ///
    /// # Examples
    ///
    /// In this example, both ends of a channel concurrently interact with its split send/receive
    /// halves. If the underlying channel implementation allows for parallelism, this simultaneous
    /// interaction can be faster than sequentially sending data back and forth.
    ///
    /// ```
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type SendAndRecv = Split<Send<Vec<usize>>, Recv<String>>;
    ///
    /// let (c1, c2) = SendAndRecv::channel(|| backend::mpsc::channel(1));
    ///
    /// // Spawn a thread to simultaneously send a `Vec<usize>` and receive a `String`:
    /// let t1 = tokio::spawn(async move {
    ///     let (tx, rx) = c1.split();
    ///     let send_vec = tokio::spawn(async move {
    ///         tx.send(vec![1, 2, 3, 4, 5]).await?;
    ///         Ok::<_, backend::mpsc::Error>(())
    ///     });
    ///     let recv_string = tokio::spawn(async move {
    ///         let (string, _) = rx.recv().await?;
    ///         Ok::<_, backend::mpsc::Error>(string)
    ///     });
    ///     send_vec.await.unwrap()?;
    ///     let string = recv_string.await.unwrap()?;
    ///     Ok::<_, backend::mpsc::Error>(string)
    /// });
    ///
    /// // Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
    /// // the task above:
    /// let (tx, rx) = c2.split();
    /// let send_string = tokio::spawn(async move {
    ///     tx.send("Hello!".to_string()).await?;
    ///     Ok::<_, backend::mpsc::Error>(())
    /// });
    /// let recv_vec = tokio::spawn(async move {
    ///     let (vec, _) = rx.recv().await?;
    ///     Ok::<_, backend::mpsc::Error>(vec)
    /// });
    ///
    /// // Examine the result values:
    /// send_string.await??;
    /// let vec = recv_vec.await??;
    /// let string = t1.await??;
    /// assert_eq!(vec, &[1, 2, 3, 4, 5]);
    /// assert_eq!(string, "Hello!");
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn split(
        self,
    ) -> (
        Chan<Tx, Unavailable<Rx>, Q::Action, Q::Env>,
        Chan<Unavailable<Tx>, Rx, R::Action, R::Env>,
    )
    {
        let (tx, rx) = self.unwrap();
        let unavailable = Unavailable::new();
        let tx_only = unsafe { Chan::with_env(tx, unavailable.clone()) };
        let rx_only = unsafe { Chan::with_env(unavailable.cast(), rx) };
        (tx_only, rx_only)
    }
}

impl<Tx, Rx, E, P> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Reunite the transmit-only and receive-only channels resulting from a call to [`Chan::split`]
    /// into a single channel.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type SplitEnds = Split<End, End>;
    ///
    /// let (c1, c2) = SplitEnds::channel(|| backend::mpsc::channel(1));
    ///
    /// let (tx1, rx1) = c1.split();
    /// let (tx2, rx2) = c2.split();
    /// let c1 = Chan::unsplit(tx1, rx1);
    /// let c2 = Chan::unsplit(tx2, rx2);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// If the two channels given as input did not result from the same call to [`Chan::split`], a
    /// panic results, since rewiring channels to be non-bidirectional can violate the session
    /// typing guarantee. If instead of the above, we did the following, `unsplit` would panic:
    ///
    /// ```should_panic
    /// # use dialectic::*;
    /// # use dialectic::constants::*;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type SplitEnds = Split<End, End>;
    /// #
    /// let (c1, c2) = SplitEnds::channel(|| backend::mpsc::channel(1));
    ///
    /// let (tx1, rx1) = c1.split();
    /// let (tx2, rx2) = c2.split();
    /// let c1 = Chan::unsplit(tx1, rx2); // <-- pairing tx from c1 with rx from c2
    /// let c2 = Chan::unsplit(tx2, rx1); // <-- pairing tx from c2 with rx from c1
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn unsplit<Q, R>(
        tx: Chan<Tx, Unavailable<Rx>, Q, E>,
        rx: Chan<Unavailable<Tx>, Rx, R, E>,
    ) -> Self
    where
        Q: Actionable<E, Action = P, Env = E>,
        R: Actionable<E, Action = P, Env = E>,
    {
        if Arc::ptr_eq(&tx.rx.0, &rx.tx.0) {
            unsafe { Chan::with_env(tx.unwrap().0, rx.unwrap().1) }
        } else {
            panic!("Unrelated channels passed to `Chan::unsplit`")
        }
    }
}

impl<Tx, Rx, E, P> Chan<Tx, Rx, P, E>
where
    P: Actionable<E>,
    E: Environment,
    E::Dual: Environment,
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
            tx,
            rx,
            environment: PhantomData,
            protocol: PhantomData,
        }
    }

    /// Unwrap a channel into its transmit and receive ends, exiting the regimen of session typing,
    /// potentially before the end of the session. **Prefer [`Chan::close`] to this method if you
    /// mean to unwrap a channel at the end of its session.**
    ///
    /// # Errors
    ///
    /// If this function is used before the end of a session, it may result in errors when the other
    /// end of the channel attempts to continue the session.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// let (c1, c2) = <Send<String>>::channel(backend::mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.unwrap();
    /// let (tx2, rx2) = c2.unwrap();
    /// ```
    pub fn unwrap(self) -> (Tx, Rx) {
        let tx = self.tx;
        let rx = self.rx;
        (tx, rx)
    }

    /// Create a new channel with an arbitrary environment. This is equivalent to casting a new
    /// channel to an arbitrary environment. Use with care!
    #[must_use]
    unsafe fn with_env(tx: Tx, rx: Rx) -> Chan<Tx, Rx, P, E> {
        Chan {
            tx,
            rx,
            environment: PhantomData,
            protocol: PhantomData,
        }
    }
}
