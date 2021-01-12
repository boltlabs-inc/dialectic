//! The [`CanonicalChan`] type is defined here. Typically, you don't need to import this module, and
//! should use the [`Chan`](super::Chan) type synonym instead.
use std::{
    convert::TryInto,
    marker::{self, PhantomData},
    sync::Arc,
};

use crate::backend::*;
use crate::prelude::*;
use crate::Chan;
use crate::{Available, Branches, SeqError, Unavailable, UnsplitError};
use futures::Future;
use tuple::{HasLength, List, Tuple};

/// A bidirectional communications channel using the session type `P` over the connections `Tx` and
/// `Rx`. ⚠️ **Important: in type signatures, always write the type synonym [`Chan`](crate::Chan),
/// not [`CanonicalChan`] directly.** [Read more
/// here.](crate::canonical::CanonicalChan#technical-notes-on-canonicity-tldr-always-write-chan)
///
/// The fourth `E` parameter to a [`CanonicalChan`] is a type-level list describing the *session
/// environment* of the channel: the stack of [`Loop`]s the channel has entered. When a loop
/// repeats, the next session type is retrieved by selecting the `N`th element of this list.
///
/// # Creating new `Chan`s: use [`NewSession`](crate::NewSession)
///
/// To construct a new `Chan`, use one of the static methods of [`NewSession`](crate::NewSession) on
/// the session type for which you want to create a channel. Here, we create two `Chan`s with the
/// session type `Send<String>` and its dual `Recv<String>`, wrapping an underlying bidirectional
/// transport built from a pair of [`tokio::sync::mpsc::channel`]s:
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::backend::mpsc;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Make a pair of channels:
/// // - `c1` with the session type `Send<String>`, and
/// // - `c2` with the dual session type `Recv<String>`
/// let (c1, c2) = <Send<String>>::channel(|| mpsc::channel(1));
/// # Ok(())
/// # }
/// ```
///
/// If you already have a sender and receiver and want to wrap them in a `Chan`, use the
/// [`wrap`](crate::NewSession::wrap) method for a session type. This is useful, for example, if
/// you're talking to another process over a network connection, where it's not possible to build
/// both halves of the channel on one computer, and instead each computer will wrap one end of the
/// connection:
///
/// ```
/// # use dialectic::prelude::*;
/// # use dialectic::backend::mpsc;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let (tx, rx) = mpsc::channel(1);
/// let c = <Send<String>>::wrap(tx, rx);
/// # Ok(())
/// # }
/// ```
///
/// # Technical notes on canonicity: TL;DR: always write `Chan`
///
/// In Dialectic, operations are liberally available on [`Chan`](super::Chan)s wherever they make
/// sense. For instance, it's valid to call [`send`](CanonicalChan::send) on a channel which was
/// created using the session type `Loop<Send<String>>`, even though the type does not literally
/// begin with `Send<String>`.
///
/// A `CanonicalChan` always has a session type which is syntactically a real action: it will never
/// be [`Loop`], [`Continue`], or [`Break`] (or, when inside a [`Loop`], it will never be [`Done`]).
/// Every action available on a [`CanonicalChan`] "fast-forwards" through such control operators,
/// yielding a [`CanonicalChan`] that corresponds to the next real action available.
///
/// While this design means greater flexibility and concision in writing session-typed code, it can
/// be confusing in the case where you want to explicitly write out the session type of a channel,
/// because the automatic canonicalization can mean a [`Chan`](super::Chan) does not have the type
/// you might think it does.
///
/// ⚠️ **The problem:** Suppose you wanted to explicitly annotate the type of a new channel:
///
/// ```compile_fail
/// # use dialectic::prelude::*;
/// # use dialectic::backend::mpsc;
/// use dialectic::canonical::CanonicalChan;
///
/// type P = Loop<Send<String>>;
/// let (c1, c2): (CanonicalChan<_, _, P, ()>, _) = P::channel(mpsc::unbounded_channel);
/// ```
///
/// This fails to typecheck, returning several errors (abridged for clarity):
///
/// ```text
/// error[E0271]: type mismatch resolving `<Loop<Send<String>> as Actionable>::Action == Loop<Send<String>>`
///    = note: expected struct `Loop<Send<_>>`
///               found struct `Send<_>`
///
/// error[E0271]: type mismatch resolving `<Loop<Send<String>> as Actionable>::Env == ()`
///    = note: expected unit type `()`
///                    found type `(Send<String>, ())`
/// ```
///
/// These errors indicate that the returned [`CanonicalChan`] from `P::channel` *does not* have the
/// session type `P` and the initial empty environment `E = ()`, as annotated. Instead, it has the
/// session type and environment corresponding to the *inside* of the `Loop`, which are the
/// *canonical* session type and environment for `P`.
///
/// 💡 **Do this instead:** When annotating the types of channels, prefer the type synonym
/// [`Chan`](super::Chan), which computes the correct [`CanonicalChan`] type for a given (possibly
/// non-canonical) session type. Using [`Chan`](super::Chan) instead of [`CanonicalChan`], we can
/// correctly annotate a newly created channel of any session type:
///
/// ```
/// # use dialectic::prelude::*;
/// # use dialectic::backend::mpsc;
/// #
/// type P = Loop<Send<String>>;
/// let (c1, c2): (Chan<_, _, P>, Chan<_, _, <P as Session>::Dual>) =
///     P::channel(mpsc::unbounded_channel);
/// ```
#[derive(Debug)]
#[must_use = "Dropping a channel before finishing its session type will result in a panic"]
pub struct CanonicalChan<Tx, Rx, P: Actionable<E, Action = P, Env = E>, E: Environment = ()> {
    tx: Tx,
    rx: Rx,
    session: PhantomData<(P, E)>,
    identity: Arc<()>,
}

impl<Tx, Rx> CanonicalChan<Tx, Rx, Done, ()> {
    /// Close a finished session, returning the wrapped connections used during the session.
    ///
    /// This function does not do cleanup on the actual underlying connections; they are passed back
    /// to the caller who may either continue to use them outside the session typing context or
    /// clean them up as appropriate.
    ///
    /// # Examples
    ///
    /// Starting with a channel whose session type is already `Done`, we can immediately close the
    /// channel.
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = Done::channel(mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.close();
    /// let (tx2, rx2) = c2.close();
    /// # }
    /// ```
    ///
    /// However, if the channel's session type is *not* `Done`, it is a type error to attempt to
    /// close the channel and retrieve its underlying sender and receiver. The following code **will
    /// not compile**:
    ///
    /// ```compile_fail
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = <Loop<Send<String, Continue>>>::channel(mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.close();
    /// let (tx2, rx2) = c2.close();
    /// # }
    /// ```
    ///
    /// If you *really* want to destruct a channel before the end of its session, use
    /// [`unwrap`](CanonicalChan::unwrap), but beware that this may cause the party on the other end of the
    /// channel to throw errors due to your violation of the channel's protocol!
    pub fn close(self) -> (Tx, Rx) {
        self.unwrap()
    }
}

impl<Tx, Rx, E, T, P> CanonicalChan<Tx, Rx, Recv<T, P>, E>
where
    Rx: Receive<T>,
    T: marker::Send + 'static,
    P: Actionable<E>,
    E: Environment,
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
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Recv<String>>::channel(|| mpsc::channel(1));
    /// c2.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c1) = c1.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    pub async fn recv(mut self) -> Result<(T, Chan<Tx, Rx, P, E>), Rx::Error> {
        let result = self.rx.recv().await?;
        Ok((result, self.unchecked_cast()))
    }
}

impl<'a, Tx, Rx, E, T, P> CanonicalChan<Tx, Rx, Send<T, P>, E>
where
    P: Actionable<E>,
    E: Environment,
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
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Send<String>>::channel(|| mpsc::channel(1));
    /// c1.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c2) = c2.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    pub async fn send<Convention: CallingConvention>(
        mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Result<Chan<Tx, Rx, P, E>, <Tx as Transmit<T, Convention>>::Error>
    where
        Tx: Transmit<T, Convention>,
        T: CallBy<'a, Convention>,
        <T as CallBy<'a, Convention>>::Type: marker::Send,
    {
        self.tx.send(message).await?;
        Ok(self.unchecked_cast())
    }
}

impl<Tx, Rx, E, Choices> CanonicalChan<Tx, Rx, Choose<Choices>, E>
where
    Tx: Transmit<Choice<<Choices::AsList as HasLength>::Length>, Val>,
    Choices: Tuple,
    Choices::AsList: HasLength,
    <Choices::AsList as HasLength>::Length: marker::Send,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    Choices::AsList: EachScoped<E::Depth>,
{
    /// Actively choose to enter the `N`th protocol offered via [`offer!`](crate::offer) by the
    /// other end of the connection, alerting the other party to this choice by sending the number
    /// `N` over the channel.
    ///
    /// The choice `N` is specified as a type-level [`Unary`] number. Predefined constants for all
    /// supported numbers of choices (up to a maximum of 127) are available in the
    /// [`constants`](crate::types::unary::constants) module, each named for its corresponding
    /// decimal number prefixed with an underscore (e.g. `_0`, or `_42`).
    ///
    /// # Errors
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending the choice.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
    ///
    /// let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
    ///
    /// // Spawn a thread to offer a choice
    /// let t1 = tokio::spawn(async move {
    ///     offer!(c2 => {
    ///         _0 => { c2.recv().await?; },
    ///         _1 => { c2.send("Hello!".to_string()).await?; },
    ///     });
    ///     Ok::<_, mpsc::Error>(())
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
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type OnlyTwoChoices = Choose<(Done, Done)>;
    /// let (c1, c2) = OnlyTwoChoices::channel(|| mpsc::channel(1));
    ///
    /// // Try to choose something out of range (this doesn't typecheck)
    /// c1.choose(_2).await?;
    ///
    /// # // Wait for the offering thread to finish
    /// # t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn choose<N: Unary>(
        mut self,
        _choice: N,
    ) -> Result<Chan<Tx, Rx, <Choices::AsList as Select<N>>::Selected, E>, Tx::Error>
    where
        N: LessThan<_128>,
        Choices::AsList: Select<N>,
        <Choices::AsList as Select<N>>::Selected: Actionable<E>,
    {
        let choice = (N::VALUE as u8)
            .try_into()
            .expect("type system prevents out of range choice in `choose`");
        self.tx.send(choice).await?;
        Ok(self.unchecked_cast())
    }
}

impl<'a, Tx, Rx, E, Choices> CanonicalChan<Tx, Rx, Offer<Choices>, E>
where
    Rx: Receive<Choice<<Choices::AsList as HasLength>::Length>>,
    Choices: Tuple,
    Choices::AsList: HasLength,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    Choices::AsList: EachActionable<E>,
    Choices::AsList: EachScoped<E::Depth>,
{
    /// Offer the choice of one or more protocols to the other party, and wait for them to indicate
    /// by sending a number which protocol to proceed with.
    ///
    ///💡 **Where possible, prefer the [`offer!`](crate::offer) macro**. This has the benefit of
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
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
    ///
    /// let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
    ///
    /// // Spawn a thread to offer a choice
    /// let t1 = tokio::spawn(async move {
    ///     match c2.offer().await?.case() {
    ///         Ok(c2) => { c2.recv().await?; },
    ///         Err(rest) => match rest.case() {
    ///             Ok(c2) => { c2.send("Hello!".to_string()).await?; },
    ///             Err(rest) => rest.empty_case(),
    ///         }
    ///     }
    ///     Ok::<_, mpsc::Error>(())
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
    /// # use dialectic::prelude::*;
    /// # use dialectic::backend::mpsc;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
    /// #
    /// # let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
    /// #
    /// # // Spawn a thread to offer a choice
    /// # let t1 = tokio::spawn(async move {
    /// offer!(c2 => {
    ///     _0 => { c2.recv().await?; },
    ///     _1 => { c2.send("Hello!".to_string()).await?; },
    /// });
    /// # Ok::<_, mpsc::Error>(())
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
    pub async fn offer(self) -> Result<Branches<Tx, Rx, Choices, E>, Rx::Error> {
        let (tx, mut rx) = self.unwrap();
        let variant = rx.recv().await?.into();
        Ok(Branches {
            variant,
            tx,
            rx,
            protocols: PhantomData,
            environment: PhantomData,
        })
    }
}

impl<Tx, Rx, E, P, Q> CanonicalChan<Tx, Rx, Split<P, Q>, E>
where
    P: Actionable<E>,
    Q: Actionable<E>,
    E: Environment,
{
    /// Split a channel into transmit-only and receive-only ends which may be used concurrently and
    /// reunited (provided they reach a matching session type) using [`unsplit_with`](CanonicalChan::unsplit_with).
    ///
    /// # Examples
    ///
    /// In this example, both ends of a channel concurrently interact with its split send/receive
    /// halves. If the underlying channel implementation allows for parallelism, this simultaneous
    /// interaction can be faster than sequentially sending data back and forth.
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type SendAndRecv = Split<Send<Vec<usize>>, Recv<String>>;
    ///
    /// let (c1, c2) = SendAndRecv::channel(|| mpsc::channel(1));
    ///
    /// // Spawn a thread to simultaneously send a `Vec<usize>` and receive a `String`:
    /// let t1 = tokio::spawn(async move {
    ///     let (tx, rx) = c1.split();
    ///     let send_vec = tokio::spawn(async move {
    ///         tx.send(vec![1, 2, 3, 4, 5]).await?;
    ///         Ok::<_, mpsc::Error>(())
    ///     });
    ///     let recv_string = tokio::spawn(async move {
    ///         let (string, _) = rx.recv().await?;
    ///         Ok::<_, mpsc::Error>(string)
    ///     });
    ///     send_vec.await.unwrap()?;
    ///     let string = recv_string.await.unwrap()?;
    ///     Ok::<_, mpsc::Error>(string)
    /// });
    ///
    /// // Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
    /// // the task above:
    /// let (tx, rx) = c2.split();
    /// let send_string = tokio::spawn(async move {
    ///     tx.send("Hello!".to_string()).await?;
    ///     Ok::<_, mpsc::Error>(())
    /// });
    /// let recv_vec = tokio::spawn(async move {
    ///     let (vec, _) = rx.recv().await?;
    ///     Ok::<_, mpsc::Error>(vec)
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
    pub fn split(
        self,
    ) -> (
        Chan<Available<Tx>, Unavailable<Rx>, P, E>,
        Chan<Unavailable<Tx>, Available<Rx>, Q, E>,
    ) {
        let tx = self.tx;
        let rx = self.rx;
        let identity = self.identity;
        let tx_only = CanonicalChan::from_raw_unchecked_with_identity(
            identity.clone(),
            Available(tx),
            Unavailable::new(),
        );
        let rx_only = CanonicalChan::from_raw_unchecked_with_identity(
            identity,
            Unavailable::new(),
            Available(rx),
        );
        (tx_only, rx_only)
    }
}

impl<Tx, Rx, P, E> CanonicalChan<Available<Tx>, Unavailable<Rx>, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    /// Reunite the transmit-only and receive-only channels resulting from a call to
    /// [`split`](CanonicalChan::split) into a single channel.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Split<Done, Done>>::channel(|| mpsc::channel(1));
    /// let (tx1, rx1) = c1.split();
    /// let (tx2, rx2) = c2.split();
    ///
    /// let c1 = tx1.unsplit_with(rx1)?;
    /// let c2 = tx2.unsplit_with(rx2)?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// If the two channels given as input did not result from the same call to
    /// [`split`](CanonicalChan::split), this function returns an [`UnsplitError`] containing the two
    /// channels, since rewiring channels to be non-bidirectional can violate the session typing
    /// guarantee. If instead of the above, we did the following, `unsplit_with` would return an error:
    ///
    /// ```
    /// # use dialectic::prelude::*;
    /// # use dialectic::backend::mpsc;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type SplitEnds = Split<Done, Done>;
    /// #
    /// let (c1, c2) = SplitEnds::channel(|| mpsc::channel(1));
    /// let (tx1, rx1) = c1.split();
    /// let (tx2, rx2) = c2.split();
    ///
    /// assert!(tx1.unsplit_with(rx2).is_err()); // <-- pairing tx from c1 with rx from c2
    /// assert!(tx2.unsplit_with(rx1).is_err()); // <-- pairing tx from c2 with rx from c1
    /// # Ok(())
    /// # }
    /// ```
    pub fn unsplit_with(
        self,
        rx: CanonicalChan<Unavailable<Tx>, Available<Rx>, P, E>,
    ) -> Result<Chan<Tx, Rx, P, E>, UnsplitError<Tx, Rx, P, E>> {
        if Arc::ptr_eq(&self.identity, &rx.identity) {
            Ok(CanonicalChan::from_raw_unchecked(
                self.unwrap().0.into_inner(),
                rx.unwrap().1.into_inner(),
            ))
        } else {
            Err(UnsplitError { tx: self, rx })
        }
    }
}

impl<Tx, Rx, E, P, Q> CanonicalChan<Tx, Rx, Seq<P, Q>, E>
where
    P: Actionable<E::MakeDone>,
    Q: Actionable<E>,
    E: Environment,
{
    /// Sequence an arbitrary session `P` before another session `Q`.
    ///
    /// This operation takes as input an asynchronous closure that runs a channel for the session
    /// type `P` to completion and returns either an error `Err` or some result value `T`. The
    /// result of this (provided that no errors occurred during `P`) is a channel ready to execute
    /// the session type `Q`.
    ///
    /// ⚠️ **Important:** the closure passed to [`seq`](CanonicalChan::seq) must, if it returns
    /// successfully, always return *the same* [`Chan`] it was given, not merely one with the same
    /// type. Doing otherwise is always a programming mistake, since if it were permissible to
    /// return a different [`Chan`], it would be possible to violate session type safety. Returning
    /// anything other than the precise [`Chan`] given to the closure will result in a runtime
    /// [`SeqError`].
    ///
    /// # Errors
    ///
    /// This function returns an `Err` if the closure returns an `Err`. Additionally, the returned
    /// channel may instead be a [`SeqError`] if the closure erroneously returned a channel which
    /// was not the exact same channel as was given to the closure as input.
    ///
    /// # Examples
    ///
    /// In the simplest uses, this can be used to cleanly modularize a session-typed program by
    /// splitting it up into independent subroutines:
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// // TODO: Finish this example!
    ///
    /// ```
    ///
    /// More generally, this allows for arbitrary context-free session types, by permitting multiple
    /// [`Continue`]s to be sequenced together.
    pub async fn seq<T, Err, F, Fut>(
        self,
        first: F,
    ) -> Result<(T, Result<Chan<Tx, Rx, Q, E>, SeqError>), Err>
    where
        F: FnOnce(Chan<Tx, Rx, P, E::MakeDone>) -> Fut,
        Fut: Future<Output = Result<(T, Chan<Tx, Rx, Done>), Err>>,
    {
        let identity = self.identity.clone();
        let (res, chan) = first(self.unchecked_cast()).await?;
        if Arc::ptr_eq(&identity, &chan.identity) {
            Ok((res, Ok(chan.unchecked_cast())))
        } else {
            Ok((res, Err(SeqError { _private: () })))
        }
    }
}

impl<Tx, Rx, E, P> CanonicalChan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    /// Cast a channel to arbitrary new session types and environment. Use with care!
    fn unchecked_cast<F, Q>(self) -> CanonicalChan<Tx, Rx, Q, F>
    where
        F: Environment,
        Q: Actionable<F, Action = Q, Env = F>,
    {
        let identity = self.identity;
        let tx = self.tx;
        let rx = self.rx;
        CanonicalChan {
            tx,
            rx,
            session: PhantomData,
            identity,
        }
    }

    /// Unwrap a channel into its transmit and receive ends, exiting the regimen of session typing,
    /// potentially before the end of the session.
    ///
    /// **Prefer [`close`](CanonicalChan::close) to this method if you mean to unwrap a channel at
    /// the end of its session.**
    ///
    /// # Errors
    ///
    /// If this function is used before the end of a session, it may result in errors when the other
    /// end of the channel attempts to continue the session.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// let (c1, c2) = <Send<String>>::channel(mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.unwrap();
    /// let (tx2, rx2) = c2.unwrap();
    /// ```
    pub fn unwrap(self) -> (Tx, Rx) {
        let tx = self.tx;
        let rx = self.rx;
        (tx, rx)
    }

    /// Create a new channel with an arbitrary environment and session type. This is equivalent to
    /// casting a new channel to an arbitrary environment, and doesn't guarantee the environment is
    /// coherent with regard to the session type. Use with care!
    pub(crate) fn from_raw_unchecked(tx: Tx, rx: Rx) -> CanonicalChan<Tx, Rx, P, E> {
        CanonicalChan::from_raw_unchecked_with_identity(Arc::new(()), tx, rx)
    }

    /// Create a new channel with an arbitrary environment and session type, with an explicitly
    /// specified object identity pointer. This `Arc<()>` is used to track whether a channel is
    /// *identical* to another channel. Misuse of this method can lead to violation of the session
    /// type contract.
    fn from_raw_unchecked_with_identity(
        identity: Arc<()>,
        tx: Tx,
        rx: Rx,
    ) -> CanonicalChan<Tx, Rx, P, E> {
        CanonicalChan {
            tx,
            rx,
            session: PhantomData,
            identity,
        }
    }
}
