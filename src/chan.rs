//! The [`Chan`] type is defined here. Typically, you don't need to import this module, and
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

use crate::tuple::{HasLength, List, Tuple};
use crate::{backend::*, IncompleteHalf, SessionIncomplete};
use crate::{
    prelude::*,
    types::{EachHasDual, EachScoped, Select},
};
use crate::{Available, Unavailable};
use futures::Future;

/// A bidirectional communications channel using the session type `P` over the connections `Tx` and
/// `Rx`.
///
/// # Creating new `Chan`s: use [`Session`](crate::Session)
///
/// To construct a new `Chan`, use one of the static methods of [`Session`](crate::Session) on the
/// session type for which you want to create a channel. Here, we create two `Chan`s with the
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
/// [`wrap`](crate::Session::wrap) method for a session type. This is useful, for example, if
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
pub struct Chan<Tx: std::marker::Send + 'static, Rx: std::marker::Send + 'static, S: Session> {
    tx: Option<Tx>,
    rx: Option<Rx>,
    #[derivative(Debug = "ignore")]
    drop_tx: Option<Box<dyn FnOnce(bool, Tx) + std::marker::Send>>,
    #[derivative(Debug = "ignore")]
    drop_rx: Option<Box<dyn FnOnce(bool, Rx) + std::marker::Send>>,
    #[derivative(Debug = "ignore")]
    session: PhantomData<S>,
}

// This is safe because `S` is only used as a phantom type.
unsafe impl<Tx: std::marker::Send, Rx: std::marker::Send, S: Session> std::marker::Send
    for Chan<Tx, Rx, S>
{
}

impl<Tx, Rx, S> Drop for Chan<Tx, Rx, S>
where
    Tx: std::marker::Send + 'static,
    Rx: std::marker::Send + 'static,
    S: Session,
{
    fn drop(&mut self) {
        let done = TypeId::of::<<S as Session>::Action>() == TypeId::of::<Done>();
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

impl<Tx: marker::Send + 'static, Rx: marker::Send + 'static, S: Session> Chan<Tx, Rx, S> {
    /// Close a finished session, dropping the underlying connections.
    ///
    /// If called inside a future given to [`split`](Chan::split) or
    /// [`seq`](Chan::seq), the underlying connections are implicitly recovered for use in
    /// subsequent actions in the session, or if called in a future given to in
    /// [`over`](Session::over), are returned to the caller.
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
    /// close the channel. The following code will not compile:
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
    /// [`unwrap`](Chan::unwrap), but beware that this may cause the party on the other end
    /// of the channel to throw errors due to your violation of the channel's protocol!
    pub fn close(self)
    where
        S: Session<Action = Done>,
    {
        drop(self)
    }

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
    pub async fn recv<T, P>(mut self) -> Result<(T, Chan<Tx, Rx, P>), Rx::Error>
    where
        S: Session<Action = Recv<T, P>>,
        P: Session,
        Rx: Receive<T>,
    {
        let result = self.rx.as_mut().unwrap().recv().await?;
        Ok((result, self.unchecked_cast()))
    }

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
    pub async fn send<'b, Convention, T, P>(
        mut self,
        message: <T as CallBy<'b, Convention>>::Type,
    ) -> Result<Chan<Tx, Rx, P>, <Tx as Transmit<T, Convention>>::Error>
    where
        S: Session<Action = Send<T, P>>,
        P: Session,
        Convention: CallingConvention,
        Tx: Transmit<T, Convention>,
        T: CallBy<'b, Convention>,
        <T as CallBy<'b, Convention>>::Type: marker::Send,
    {
        self.tx.as_mut().unwrap().send(message).await?;
        Ok(self.unchecked_cast())
    }

    /// Actively choose to enter the `N`th protocol offered via [`offer!`](crate::offer) by the
    /// other end of the connection, alerting the other party to this choice by sending the number
    /// `N` over the channel.
    ///
    /// The choice `N` is specified as a type-level [`Unary`] number. Predefined constants for all
    /// supported numbers of choices (up to a maximum of 127) are available in the
    /// [`constants`](crate::unary::constants) module, each named for its corresponding
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
    pub async fn choose<N, Choices>(
        mut self,
        _choice: N,
    ) -> Result<Chan<Tx, Rx, <Choices::AsList as Select<N>>::Selected>, Tx::Error>
    where
        S: Session<Action = Choose<Choices>>,
        Tx: Transmit<Choice<<Choices::AsList as HasLength>::Length>, Val>,
        Choices: Tuple,
        Choices::AsList: Select<N> + HasLength + EachHasDual + EachScoped,
        <Choices::AsList as EachHasDual>::Duals: List,
        <Choices::AsList as Select<N>>::Selected: Session,
        N: Unary + LessThan<_128>,
    {
        let choice = (N::VALUE as u8)
            .try_into()
            .expect("type system prevents out of range choice in `choose`");
        self.tx.as_mut().unwrap().send(choice).await?;
        Ok(self.unchecked_cast())
    }

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
    pub async fn offer<Choices>(mut self) -> Result<Branches<Tx, Rx, Choices>, Rx::Error>
    where
        S: Session<Action = Offer<Choices>>,
        Rx: Receive<Choice<<Choices::AsList as HasLength>::Length>>,
        Choices: Tuple + 'static,
        Choices::AsList: HasLength + EachScoped + EachHasDual,
        <Choices::AsList as EachHasDual>::Duals: List,
        _0: LessThan<<Choices::AsList as HasLength>::Length>,
    {
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

    /// Split a channel into transmit-only and receive-only ends and manipulate them, potentially
    /// concurrently, in the given closure.
    ///
    /// To use the channel as a reunited whole after it has been split, combine this operation with
    /// [`seq`](Chan::seq) to sequence further operations after it.
    ///
    /// # Errors
    ///
    /// The closure must *finish* the session for both the send-only and receive-only ends of the
    /// channel and drop or [`close`](Chan::close) each end *before* the future completes.
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
    pub async fn split<T, E, P, Q, F, Fut>(
        mut self,
        with_parts: F,
    ) -> Result<(T, Result<Chan<Tx, Rx, Done>, SessionIncomplete<Tx, Rx>>), E>
    where
        S: Session<Action = Split<P, Q>>,
        P: Session,
        Q: Session,
        F: FnOnce(
            Chan<Available<Tx>, Unavailable<Rx>, P>,
            Chan<Unavailable<Tx>, Available<Rx>, Q>,
        ) -> Fut,
        Fut: Future<Output = Result<T, E>>,
    {
        use IncompleteHalf::*;
        use SessionIncomplete::*;

        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        let drop_tx = self.drop_tx.take().unwrap();
        let drop_rx = self.drop_rx.take().unwrap();
        let ((result, maybe_rx), maybe_tx) =
            over::<P, _, _, _, _, _, _>(Available(tx), Unavailable::new(), |tx_only| async move {
                over::<Q, _, _, _, _, _, _>(
                    Unavailable::new(),
                    Available(rx),
                    |rx_only| async move { with_parts(tx_only, rx_only).await },
                )
                .await
            })
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
            maybe_tx_rx.map(|(tx, rx)| Chan {
                tx: Some(tx),
                rx: Some(rx),
                drop_tx: Some(drop_tx),
                drop_rx: Some(drop_rx),
                session: PhantomData,
            }),
        ))
    }

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
    /// [`close`](Chan::close) on the channel before returning from the future, because
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
    /// [`seq`](Chan::seq) operator is roughly equivalent to its `@=` operator, and the
    /// [`Seq`] type is equivalent to `;`.
    pub async fn seq<T, E, P, Q, F, Fut>(
        mut self,
        first: F,
    ) -> Result<(T, Result<Chan<Tx, Rx, Q>, SessionIncomplete<Tx, Rx>>), E>
    where
        S: Session<Action = Seq<P, Q>>,
        P: Session,
        Q: Session,
        F: FnOnce(Chan<Tx, Rx, P>) -> Fut,
        Fut: Future<Output = Result<T, E>>,
    {
        let tx = self.tx.take().unwrap();
        let rx = self.rx.take().unwrap();
        let drop_tx = self.drop_tx.take().unwrap();
        let drop_rx = self.drop_rx.take().unwrap();
        let (result, maybe_chan) = over::<P, _, _, _, _, _, _>(tx, rx, first).await?;
        Ok((
            result,
            maybe_chan.map(|(tx, rx)| Chan {
                tx: Some(tx),
                rx: Some(rx),
                drop_tx: Some(drop_tx),
                drop_rx: Some(drop_rx),
                session: PhantomData,
            }),
        ))
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

    /// Cast a channel to arbitrary new session types and environment. Use with care!
    fn unchecked_cast<Q>(mut self) -> Chan<Tx, Rx, Q>
    where
        Q: Session,
    {
        Chan {
            tx: self.tx.take(),
            rx: self.rx.take(),
            drop_tx: self.drop_tx.take(),
            drop_rx: self.drop_rx.take(),
            session: PhantomData,
        }
    }

    /// Create a new channel with an arbitrary environment and session type. This is equivalent to
    /// casting a new channel to an arbitrary environment, and doesn't guarantee the environment is
    /// coherent with regard to the session type. Use with care!
    pub(crate) fn from_raw_unchecked(tx: Tx, rx: Rx) -> Chan<Tx, Rx, S> {
        Chan {
            tx: Some(tx),
            rx: Some(rx),
            drop_tx: Some(Box::new(|_, _| {})),
            drop_rx: Some(Box::new(|_, _| {})),
            session: PhantomData,
        }
    }
}

/// The implementation of `Session::over`, generalized to any environment (unlike the public
/// `over` function which only works in the empty environment). This is not safe to export because
/// nonsense environments could be specified.
pub(crate) fn over<P, Tx, Rx, T, E, F, Fut>(tx: Tx, rx: Rx, with_chan: F) -> Over<Tx, Rx, T, E, Fut>
where
    P: Session,
    Tx: std::marker::Send + 'static,
    Rx: std::marker::Send + 'static,
    F: FnOnce(Chan<Tx, Rx, P>) -> Fut,
    Fut: Future<Output = Result<T, E>>,
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
    let chan = Chan {
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

/// The future returned by [`Session::over`] (see its documentation for details).
#[pin_project]
#[derive(Debug)]
pub struct Over<Tx, Rx, T, E, Fut>
where
    Fut: Future<Output = Result<T, E>>,
{
    /// The destination for the reclaimed transmit half after drop.
    reclaimed_tx: Arc<Mutex<Result<Tx, IncompleteHalf<Tx>>>>,
    /// The destination for the reclaimed receive half after drop.
    reclaimed_rx: Arc<Mutex<Result<Rx, IncompleteHalf<Rx>>>>,
    /// The wrapped future itself.
    #[pin]
    future: Fut,
}

/// The result of [`offer`](Chan::offer): an enumeration of the possible new channel states
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
    Choices::AsList: EachScoped + EachHasDual,
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
    Choices::AsList: EachScoped + EachHasDual,
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
    P: Session,
    Ps: EachScoped + EachHasDual + List,
{
    /// Check if the selected protocol in this [`Branches`] was `P`. If so, return the corresponding
    /// channel; otherwise, return all the other possibilities.
    #[must_use = "all possible choices must be handled (add cases to match the type of this `Offer<...>`)"]
    pub fn case(mut self) -> Result<Chan<Tx, Rx, P>, Branches<Tx, Rx, Ps::AsTuple>> {
        let variant = self.variant;
        let tx = self.tx.take();
        let rx = self.rx.take();
        let drop_tx = self.drop_tx.take();
        let drop_rx = self.drop_rx.take();
        if variant == 0 {
            Ok(Chan {
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

impl<'a, Tx, Rx> Branches<Tx, Rx, ()>
where
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
{
    /// Eliminate an empty [`Branches`], returning any type. Any code in which this function can be
    /// called is unreachable, because it's impossible to construct an empty [`Branches`].
    pub fn empty_case<T>(self) -> T {
        unreachable!("empty `Branches` cannot be constructed")
    }
}