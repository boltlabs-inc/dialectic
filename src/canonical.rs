//! The [`CanonicalChan`] type is defined here. Typically, you don't need to import this module, and
//! should use the [`Chan`](super::Chan) type synonym instead.
use std::{
    any::TypeId,
    convert::TryInto,
    marker::{self, PhantomData},
    mem,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll},
};

use pin_project::pin_project;

use crate::prelude::*;
use crate::Chan;
use crate::{backend::*, IncompleteHalf, SessionIncomplete};
use crate::{Available, Unavailable};
use futures::Future;
use tuple::{HasLength, List, Tuple};

/// A bidirectional communications channel using the session type `P` over the connections `Tx` and
/// `Rx`. ⚠️ **Important: in type signatures, always write the type synonym [`Chan`](crate::Chan),
/// not [`CanonicalChan`] directly.** [Read more
/// here.](crate::Chan#technical-notes-on-canonicity-tldr-always-write-chan)
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
#[derive(Derivative)]
#[derivative(Debug)]
#[must_use]
pub struct CanonicalChan<
    Tx: std::marker::Send + 'static,
    Rx: std::marker::Send + 'static,
    P: Actionable<E, Action = P, Env = E>,
    E: Environment = (),
> {
    tx: Option<Tx>,
    rx: Option<Rx>,
    #[derivative(Debug = "ignore")]
    drop_tx: Option<Box<dyn FnOnce(bool, Tx) + std::marker::Send>>,
    #[derivative(Debug = "ignore")]
    drop_rx: Option<Box<dyn FnOnce(bool, Rx) + std::marker::Send>>,
    #[derivative(Debug = "ignore")]
    session: PhantomData<(P, E)>,
}

impl<Tx, Rx, P, E> Drop for CanonicalChan<Tx, Rx, P, E>
where
    Tx: std::marker::Send + 'static,
    Rx: std::marker::Send + 'static,
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    fn drop(&mut self) {
        let done =
            TypeId::of::<P>() == TypeId::of::<Done>() && TypeId::of::<E>() == TypeId::of::<()>();
        let tx = self.tx.take();
        let rx = self.rx.take();
        let drop_tx = self.drop_tx.take();
        let drop_rx = self.drop_rx.take();
        if let (Some(drop_tx), Some(tx)) = (drop_tx, tx) {
            drop_tx(done, tx);
        }
        if let (Some(drop_rx), Some(rx)) = (drop_rx, rx) {
            drop_rx(done, rx);
        }
    }
}

impl<Tx: marker::Send + 'static, Rx: marker::Send + 'static> CanonicalChan<Tx, Rx, Done, ()> {
    /// Close a finished session, dropping the underlying connections.
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
    /// c1.close();
    /// c2.close();
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
    /// c1.close();
    /// c2.close();
    /// # }
    /// ```
    ///
    /// If you *really* want to destruct a channel before the end of its session, use
    /// [`unwrap`](CanonicalChan::unwrap), but beware that this may cause the party on the other end
    /// of the channel to throw errors due to your violation of the channel's protocol!
    pub fn close(self) {
        drop(self)
    }
}

impl<'a, Tx, Rx, E, T, P> CanonicalChan<Tx, Rx, Recv<T, P>, E>
where
    Tx: marker::Send + 'static,
    Rx: Receive<T> + marker::Send + 'static,
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
        let result = self.rx.as_mut().unwrap().recv().await?;
        Ok((result, self.unchecked_cast()))
    }
}

impl<'a, Tx, Rx, E, T: 'static, P> CanonicalChan<Tx, Rx, Send<T, P>, E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
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
    pub async fn send<'b, Convention: CallingConvention>(
        mut self,
        message: <T as CallBy<'b, Convention>>::Type,
    ) -> Result<Chan<Tx, Rx, P, E>, <Tx as Transmit<T, Convention>>::Error>
    where
        Tx: Transmit<T, Convention>,
        T: CallBy<'b, Convention>,
        <T as CallBy<'b, Convention>>::Type: marker::Send,
    {
        self.tx.as_mut().unwrap().send(message).await?;
        Ok(self.unchecked_cast())
    }
}

impl<Tx, Rx, E, Choices: 'static> CanonicalChan<Tx, Rx, Choose<Choices>, E>
where
    Tx: Transmit<Choice<<Choices::AsList as HasLength>::Length>, Val> + marker::Send + 'static,
    Rx: marker::Send + 'static,
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
        self.tx.as_mut().unwrap().send(choice).await?;
        Ok(self.unchecked_cast())
    }
}

impl<'a, Tx, Rx, E, Choices: 'static> CanonicalChan<Tx, Rx, Offer<Choices>, E>
where
    Tx: marker::Send + 'static,
    Rx: Receive<Choice<<Choices::AsList as HasLength>::Length>> + marker::Send + 'static,
    Choices: Tuple,
    Choices::AsList: HasLength,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    Choices::AsList: EachActionable<E>,
    Choices::AsList: EachScoped<E::Depth>,
    _0: LessThan<<Choices::AsList as HasLength>::Length>,
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
    pub async fn offer(mut self) -> Result<Branches<Tx, Rx, Choices, E>, Rx::Error> {
        let tx = self.tx.take();
        let mut rx = self.rx.take();
        let drop_tx = self.drop_tx.take();
        let drop_rx = self.drop_rx.take();
        let variant = rx.as_mut().unwrap().recv().await?.into();
        Ok(Branches {
            variant,
            tx,
            rx,
            drop_tx,
            drop_rx,
            protocols: PhantomData,
            environment: PhantomData,
        })
    }
}

impl<'a, Tx, Rx, E, P, Q> CanonicalChan<Tx, Rx, Split<P, Q>, E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    P: Actionable<E>,
    Q: Actionable<E>,
    E: Environment,
{
    /// Split a channel into transmit-only and receive-only ends and manipulate them, potentially
    /// concurrently, in the given closure.
    ///
    /// To use the channel as a reunited whole after it has been split, combine this operation with
    /// [`seq`](CanonicalChan::seq) to sequence further operations after it.
    ///
    /// # Errors
    ///
    /// The closure must *finish* the session for both the send-only and receive-only ends of the
    /// channel and drop or [`close`](CanonicalChan::close) each end *before* the future completes.
    /// If either end is dropped before finishing its session, or is not closed after finishing its
    /// session, a [`SessionIncomplete`] error will be returned instead of a finished channel.
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
    ///     c1.split(|tx, rx| async move {
    ///         let send_vec = tokio::spawn(async move {
    ///             tx.send(vec![1, 2, 3, 4, 5]).await?;
    ///             Ok::<_, mpsc::Error>(())
    ///         });
    ///         let recv_string = tokio::spawn(async move {
    ///             let (string, _) = rx.recv().await?;
    ///             Ok::<_, mpsc::Error>(string)
    ///         });
    ///         send_vec.await.unwrap()?;
    ///         let string = recv_string.await.unwrap()?;
    ///         Ok::<_, mpsc::Error>(string)
    ///     }).await
    /// });
    ///
    /// // Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
    /// // the task above:
    /// c2.split(|tx, rx| async move {
    ///     let send_string = tokio::spawn(async move {
    ///         tx.send("Hello!".to_string()).await?;
    ///         Ok::<_, mpsc::Error>(())
    ///     });
    ///     let recv_vec = tokio::spawn(async move {
    ///         let (vec, _) = rx.recv().await?;
    ///         Ok::<_, mpsc::Error>(vec)
    ///     });
    ///
    ///     // Examine the result values:
    ///     send_string.await??;
    ///     let vec = recv_vec.await??;
    ///     let string = t1.await??.0;
    ///     assert_eq!(vec, &[1, 2, 3, 4, 5]);
    ///     assert_eq!(string, "Hello!");
    ///
    ///     Ok::<_, Box<dyn std::error::Error>>(())
    /// }).await?;
    /// #
    /// # Ok(())
    /// # }
    /// ```
    pub async fn split<T, Err, F, Fut>(
        mut self,
        with_parts: F,
    ) -> Result<(T, Result<Chan<Tx, Rx, Done>, SessionIncomplete<Tx, Rx>>), Err>
    where
        F: FnOnce(
            Chan<Available<Tx>, Unavailable<Rx>, P, E>,
            Chan<Unavailable<Tx>, Available<Rx>, Q, E>,
        ) -> Fut,
        Fut: Future<Output = Result<T, Err>>,
    {
        use IncompleteHalf::*;
        use SessionIncomplete::*;

        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        let drop_tx = self.drop_tx.take().unwrap();
        let drop_rx = self.drop_rx.take().unwrap();
        let ((result, maybe_rx), maybe_tx) = over::<P, E, _, _, _, _, _, _>(
            Available(tx),
            Unavailable::new(),
            |tx_only| async move {
                over::<Q, E, _, _, _, _, _, _>(
                    Unavailable::new(),
                    Available(rx),
                    |rx_only| async move { with_parts(tx_only, rx_only).await },
                )
                .await
            },
        )
        .await?;
        // Unpack and repack the resultant tx and rx or SessionIncomplete to eliminate
        // Available/Unavailable and maximize possible returned things (it's fine to drop the
        // Unavailable end of something if for some reason you split twice)
        let maybe_tx_rx: Result<(Tx, Rx), SessionIncomplete<Tx, Rx>> = match (
            maybe_tx
                .map(|(tx, _)| Ok(tx))
                .unwrap_or_else(|incomplete| incomplete.into_halves().0),
            maybe_rx
                .map(|(_, rx)| Ok(rx))
                .unwrap_or_else(|incomplete| incomplete.into_halves().1),
        ) {
            (Ok(Available(tx)), Ok(Available(rx))) => Ok((tx, rx)),
            (Ok(Available(tx)), Err(Unclosed)) => Err(RxHalf { tx, rx: Unclosed }),
            (Err(Unclosed), Ok(Available(rx))) => Err(TxHalf { tx: Unclosed, rx }),
            (Ok(Available(tx)), Err(Unfinished(Available(rx)))) => Err(RxHalf {
                tx,
                rx: Unfinished(rx),
            }),
            (Err(Unfinished(Available(tx))), Ok(Available(rx))) => Err(TxHalf {
                tx: Unfinished(tx),
                rx,
            }),
            (Err(Unfinished(Available(tx))), Err(Unclosed)) => Err(BothHalves {
                tx: Unfinished(tx),
                rx: Unclosed,
            }),
            (Err(Unclosed), Err(Unfinished(Available(rx)))) => Err(BothHalves {
                tx: Unclosed,
                rx: Unfinished(rx),
            }),
            (Err(Unclosed), Err(Unclosed)) => Err(BothHalves {
                tx: Unclosed,
                rx: Unclosed,
            }),
            (Err(Unfinished(Available(tx))), Err(Unfinished(Available(rx)))) => Err(BothHalves {
                tx: Unfinished(tx),
                rx: Unfinished(rx),
            }),
        };
        Ok((
            result,
            maybe_tx_rx.map(|(tx, rx)| CanonicalChan {
                tx: Some(tx),
                rx: Some(rx),
                drop_tx: Some(drop_tx),
                drop_rx: Some(drop_rx),
                session: PhantomData,
            }),
        ))
    }
}

impl<'a, Tx, Rx, E, P, Q> CanonicalChan<Tx, Rx, Seq<P, Q>, E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    P: Actionable<E>,
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
    /// # Errors
    ///
    /// The closure must *finish* the session `P` on the channel given to it and *drop* the finished
    /// channel before the future returns. If the channel is dropped before completing `P` or is not
    /// dropped after completing `P`, a [`SessionIncomplete`] error will be returned instead of a
    /// channel for `Q`. The best way to ensure this error does not occur is to call
    /// [`close`](CanonicalChan::close) on the channel before returning from the future, because
    /// this statically checks that the session is complete and drops the channel.
    ///
    /// Additionally, this function returns an `Err` if the closure returns an `Err`.
    ///
    /// # Examples
    ///
    /// This can be used to cleanly modularize a session-typed program by splitting it up into
    /// independent subroutines:
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Seq<Send<String>, Send<String>>>::channel(mpsc::unbounded_channel);
    ///
    /// let ((), c1_result) = c1.seq(|c| async move {
    ///     let c = c.send("Hello!".to_string()).await?;
    ///     // Because we're done with this subroutine, we can "close" the channel here, but it
    ///     // will remain open to the calling context so it can run the rest of the session:
    ///     c.close();
    ///     Ok::<_, mpsc::Error>(())
    /// }).await?;
    /// let c1 = c1_result?;
    ///
    /// let c1 = c1.send("World!".to_string()).await?;
    /// c1.close();
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## An advanced example: context-free sessions
    ///
    /// More generally, [`Seq`] allows for arbitrary **context-free session types**, by permitting
    /// multiple [`Continue`]s to be sequenced together. In the following example, we define and
    /// implement a session type for valid operations on a stack: that is, the session type
    /// *statically* prevents programs that would attempt to pop from an empty stack. This session
    /// type would not be expressible without [`Seq`], because it requires a point of recursion that
    /// is not at the end of a session type.
    ///
    /// ```
    /// # use dialectic::prelude::*;
    /// # use dialectic::backend::mpsc;
    /// use std::{marker, error::Error, fmt::Debug, future::Future, pin::Pin, any::Any};
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn Error>> {
    /// type Stack<T> =
    ///     Loop<Offer<(Break, Recv<T, Seq<Continue, Send<T, Continue>>>)>>;
    ///
    /// // A server over the `mpsc` backend for the `Stack<T>` protocol
    /// fn stack<T>(
    ///     mut chan: mpsc::Chan<Stack<T>>,
    /// ) -> Pin<Box<dyn Future<Output = Result<(), mpsc::Error>> + marker::Send>>
    /// where
    ///     T: marker::Send + 'static,
    /// {
    ///     Box::pin(async move {
    ///         loop {
    ///             chan = offer!(chan => {
    ///                 // Client doesn't want to push a value
    ///                 _0 => break chan.close(),
    ///                 // Client wants to push a value
    ///                 _1 => {
    ///                     let (t, chan) = chan.recv().await?;       // Receive pushed value
    ///                     let ((), chan) = chan.seq(stack).await?;  // Recursively do `Stack<T>`
    ///                     chan.unwrap().send(t).await?              // Send back that pushed value
    ///                 },
    ///             })
    ///         }
    ///         Ok(())
    ///     })
    /// }
    ///
    /// // A client over the `mpsc` backend for the `Stack<T>` protocol, which uses the
    /// // server's stack to reverse a given iterator
    /// fn reverse_with_stack<T>(
    ///     mut chan: mpsc::Chan<<Stack<T> as Session>::Dual>,
    ///     mut iter: impl Iterator<Item = T> + marker::Send + 'static,
    /// ) -> Pin<Box<dyn Future<Output = Result<Vec<T>, mpsc::Error>> + marker::Send>>
    /// where
    ///     T: marker::Send + 'static,
    /// {
    ///     Box::pin(async move {
    ///         if let Some(t) = iter.next() {
    ///             // If there is a value left in the iterator...
    ///             let (mut reversed, chan) =
    ///                 chan.choose(_1).await?  // Choose to push a value
    ///                     .send(t).await?     // Push the value
    ///                     // Recursively push the rest of the iterator
    ///                     .seq(|chan| reverse_with_stack(chan, iter)).await?;
    ///             let (t, chan) = chan.unwrap().recv().await?;  // Pop a value
    ///             reversed.push(t);                             // Add it to the reversed `Vec`
    ///             chan.choose(_0).await?.close();               // Choose to complete the session
    ///             Ok(reversed)
    ///         } else {
    ///             // If there are no values left in the iterator...
    ///             chan.choose(_0).await?.close();  // Choose to complete the session
    ///             Ok(vec![])
    ///         }
    ///     })
    /// }
    ///
    /// // Using the server and client above, let's reverse a list!
    /// let (server_chan, client_chan) = <Stack<usize>>::channel(|| mpsc::channel(1));
    /// let server_thread = tokio::spawn(stack(server_chan));
    /// let input = vec![1, 2, 3, 4, 5].into_iter();
    /// let result = reverse_with_stack(client_chan, input).await?;
    /// assert_eq!(result, vec![5, 4, 3, 2, 1]);
    /// server_thread.await?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// For more on context-free session types, see "Context-Free Session Type Inference" by Luca
    /// Padovani: <https://doi.org/10.1145/3229062>. When comparing with that paper, note that the
    /// [`seq`](CanonicalChan::seq) operator is roughly equivalent to its `@=` operator, and the
    /// [`Seq`] type is equivalent to `;`.
    pub async fn seq<T, Err, F, Fut>(
        mut self,
        first: F,
    ) -> Result<(T, Result<Chan<Tx, Rx, Q, E>, SessionIncomplete<Tx, Rx>>), Err>
    where
        F: FnOnce(Chan<Tx, Rx, P, E>) -> Fut,
        Fut: Future<Output = Result<T, Err>>,
    {
        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        let drop_tx = self.drop_tx.take().unwrap();
        let drop_rx = self.drop_rx.take().unwrap();
        let (result, maybe_chan) = over::<P, E, _, _, _, _, _, _>(tx, rx, first).await?;
        Ok((
            result,
            maybe_chan.map(|(tx, rx)| CanonicalChan {
                tx: Some(tx),
                rx: Some(rx),
                drop_tx: Some(drop_tx),
                drop_rx: Some(drop_rx),
                session: PhantomData,
            }),
        ))
    }
}

impl<'a, Tx: 'a, Rx: 'a, E, P> CanonicalChan<Tx, Rx, P, E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    /// Cast a channel to arbitrary new session types and environment. Use with care!
    fn unchecked_cast<F, Q>(mut self) -> CanonicalChan<Tx, Rx, Q, F>
    where
        F: Environment,
        Q: Actionable<F, Action = Q, Env = F>,
    {
        CanonicalChan {
            tx: self.tx.take(),
            rx: self.rx.take(),
            drop_tx: self.drop_tx.take(),
            drop_rx: self.drop_rx.take(),
            session: PhantomData,
        }
    }

    /// Unwrap a channel into its transmit and receive ends, exiting the regimen of session typing,
    /// potentially before the end of the session.
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
    pub fn unwrap(mut self) -> (Tx, Rx) {
        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        (tx, rx)
    }

    /// Create a new channel with an arbitrary environment and session type. This is equivalent to
    /// casting a new channel to an arbitrary environment, and doesn't guarantee the environment is
    /// coherent with regard to the session type. Use with care!
    pub(crate) fn from_raw_unchecked(tx: Tx, rx: Rx) -> CanonicalChan<Tx, Rx, P, E> {
        CanonicalChan {
            tx: Some(tx),
            rx: Some(rx),
            drop_tx: Some(Box::new(|_, _| {})),
            drop_rx: Some(Box::new(|_, _| {})),
            session: PhantomData,
        }
    }
}

/// The implementation of `NewSession::over`, generalized to any environment (unlike the public
/// `over` function which only works in the empty environment). This is not safe to export because
/// nonsense environments could be specified.
pub(crate) fn over<P, E, Tx, Rx, T, Err, F, Fut>(
    tx: Tx,
    rx: Rx,
    with_chan: F,
) -> Over<Tx, Rx, T, Err, Fut>
where
    P: Actionable<E>,
    E: Environment,
    Tx: std::marker::Send + 'static,
    Rx: std::marker::Send + 'static,
    F: FnOnce(Chan<Tx, Rx, P, E>) -> Fut,
    Fut: Future<Output = Result<T, Err>>,
{
    use IncompleteHalf::*;
    let dropped_tx = Arc::new(Mutex::new(Err(Unclosed)));
    let dropped_rx = Arc::new(Mutex::new(Err(Unclosed)));
    let reclaimed_tx = dropped_tx.clone();
    let reclaimed_rx = dropped_rx.clone();
    let drop_tx: Option<Box<dyn FnOnce(bool, Tx) + std::marker::Send>> =
        Some(Box::new(move |done, tx| {
            *dropped_tx.lock().unwrap() = if done { Ok(tx) } else { Err(Unfinished(tx)) };
        }));
    let drop_rx: Option<Box<dyn FnOnce(bool, Rx) + std::marker::Send>> =
        Some(Box::new(move |done, rx| {
            *dropped_rx.lock().unwrap() = if done { Ok(rx) } else { Err(Unfinished(rx)) };
        }));
    let chan = CanonicalChan {
        tx: Some(tx),
        rx: Some(rx),
        drop_tx,
        drop_rx,
        session: PhantomData,
    };
    Over {
        future: with_chan(chan),
        reclaimed_tx,
        reclaimed_rx,
    }
}

impl<Tx, Rx, T, E, Fut> Future for Over<Tx, Rx, T, E, Fut>
where
    Fut: Future<Output = Result<T, E>>,
{
    type Output = Result<(T, Result<(Tx, Rx), SessionIncomplete<Tx, Rx>>), E>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        use IncompleteHalf::*;
        use SessionIncomplete::*;

        let reclaimed_tx = self.reclaimed_tx.clone();
        let reclaimed_rx = self.reclaimed_rx.clone();

        self.project().future.poll(cx).map(|result| {
            let chan = match (
                mem::replace(&mut *reclaimed_tx.lock().unwrap(), Err(Unclosed)),
                mem::replace(&mut *reclaimed_rx.lock().unwrap(), Err(Unclosed)),
            ) {
                (Ok(tx), Ok(rx)) => Ok((tx, rx)),
                (Err(tx), Ok(rx)) => Err(TxHalf { tx, rx }),
                (Ok(tx), Err(rx)) => Err(RxHalf { tx, rx }),
                (Err(tx), Err(rx)) => Err(BothHalves { tx, rx }),
            };
            result.map(|result| (result, chan))
        })
    }
}

/// The future returned by [`NewSession::over`] (see its documentation for details).
#[pin_project]
#[derive(Debug)]
pub struct Over<Tx, Rx, T, E, Fut>
where
    Fut: Future<Output = Result<T, E>>,
{
    reclaimed_tx: Arc<Mutex<Result<Tx, IncompleteHalf<Tx>>>>,
    reclaimed_rx: Arc<Mutex<Result<Rx, IncompleteHalf<Rx>>>>,
    #[pin]
    future: Fut,
}

/// The result of [`offer`](CanonicalChan::offer): an enumeration of the possible new channel states
/// that could result from the offering of the tuple of protocols `Choices`.
///
/// To find out which protocol was selected by the other party, use [`Branches::case`], or better
/// yet, use the [`offer!`](crate::offer) macro to ensure you don't miss any cases.
///
/// **When possible, prefer the [`offer!`] macro over using [`Branches`] and
/// [`case`](Branches::case).**
#[derive(Derivative)]
#[derivative(Debug)]
#[must_use]
pub struct Branches<Tx, Rx, Choices, E = ()>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    Choices: Tuple + 'static,
    Choices::AsList: EachActionable<E>,
    E: Environment,
{
    variant: u8,
    tx: Option<Tx>,
    rx: Option<Rx>,
    #[derivative(Debug = "ignore")]
    drop_tx: Option<Box<dyn FnOnce(bool, Tx) + std::marker::Send>>,
    #[derivative(Debug = "ignore")]
    drop_rx: Option<Box<dyn FnOnce(bool, Rx) + std::marker::Send>>,
    #[derivative(Debug = "ignore")]
    protocols: PhantomData<Choices>,
    #[derivative(Debug = "ignore")]
    environment: PhantomData<E>,
}

impl<Tx, Rx, Choices, E> Drop for Branches<Tx, Rx, Choices, E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    Choices: Tuple + 'static,
    Choices::AsList: EachActionable<E>,
    E: Environment,
{
    fn drop(&mut self) {
        // A `Branches` is never an end-state for a channel, so we always say it wasn't done
        let done = false;
        let tx = self.tx.take();
        let rx = self.rx.take();
        let drop_tx = self.drop_tx.take();
        let drop_rx = self.drop_rx.take();
        if let (Some(drop_tx), Some(tx)) = (drop_tx, tx) {
            drop_tx(done, tx);
        }
        if let (Some(drop_rx), Some(rx)) = (drop_rx, rx) {
            drop_rx(done, rx);
        }
    }
}

impl<'a, Tx, Rx, Choices, P, Ps, E> Branches<Tx, Rx, Choices, E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    Choices: Tuple<AsList = (P, Ps)>,
    (P, Ps): List<AsTuple = Choices>,
    P: Actionable<E>,
    Ps: EachActionable<E> + List,
    E: Environment,
{
    /// Check if the selected protocol in this [`Branches`] was `P`. If so, return the corresponding
    /// channel; otherwise, return all the other possibilities.
    #[must_use = "all possible choices must be handled (add cases to match the type of this `Offer<...>`)"]
    pub fn case(
        mut self,
    ) -> Result<
        Chan<Tx, Rx, <P as Actionable<E>>::Action, <P as Actionable<E>>::Env>,
        Branches<Tx, Rx, Ps::AsTuple, E>,
    > {
        let variant = self.variant;
        let tx = self.tx.take();
        let rx = self.rx.take();
        let drop_tx = self.drop_tx.take();
        let drop_rx = self.drop_rx.take();
        if variant == 0 {
            Ok(CanonicalChan {
                tx,
                rx,
                drop_tx,
                drop_rx,
                session: PhantomData,
            })
        } else {
            Err(Branches {
                variant: variant - 1,
                tx,
                rx,
                drop_tx,
                drop_rx,
                protocols: PhantomData,
                environment: PhantomData,
            })
        }
    }
}

impl<'a, Tx, Rx, E: Environment> Branches<Tx, Rx, (), E>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
{
    /// Attempt to eliminate an empty [`Branches`], returning an error if the originating
    /// discriminant for this set of protocol choices was out of range.
    ///
    /// This function is only callable on empty [`Branches`], which under ordinary circumstances
    /// means it proves the unreachability of its calling location. However, if the other end of the
    /// channel breaks protocol, an empty [`Branches`] can in fact be constructed, and this function
    /// will then signal an error.
    pub fn empty_case<T>(self) -> T {
        unreachable!("empty `Branches` cannot be constructed")
    }
}
