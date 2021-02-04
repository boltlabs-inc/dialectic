pub use crate::chan::Over;

use super::*;
use crate::types::*;

/// The `Session` extension trait gives methods to create session-typed channels from session
/// types. These are implemented as static methods on the session type itself.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::backend::mpsc;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Send<String, Done>>::channel(mpsc::unbounded_channel);
/// // do something with these channels...
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
///
/// # Notes
///
/// The trait bounds specified for [`Session`] ensure that the session type is well-formed.
/// However, it does not ensure that all the types in the session type can be sent or received over
/// the given channels.
///
/// Valid session types must contain only "productive" recursion: they must not contain any
/// [`Continue`] directly inside the loop to which that recursion refers. For instance, attempting
/// to use the session type `Loop<Continue>` will result in a compile-time trait solver overflow.
///
/// ```compile_fail
/// use dialectic::*;
/// use dialectic::backend::mpsc;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Loop<Continue>>::channel(mpsc::unbounded_channel);
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
///
/// This results in the compiler error:
///
///
/// ```text
/// error[E0275]: overflow evaluating the requirement `Continue: Actionable<(Continue, ())>`
///   |
/// 7 | let (c1, c2) = <Loop<Continue>>::channel(backend::mpsc::unbounded_channel);
///   |                ^^^^^^^^^^^^^^^^^^^^^^
///   |
///   = help: consider adding a `#![recursion_limit="256"]` attribute to your crate
///   = note: required because of the requirements on the impl of `Actionable<()>` for `Loop<Continue>`
///   = note: required because of the requirements on the impl of `Session` for `Loop<Continue>`
/// ```
///
/// In this situation, you **should not** take `rustc`'s advice. If you add a
/// `#![recursion_limit="256"]` attribute to your crate: this will only cause the compiler to work
/// harder before giving you the same error!
///
/// What the compiler is trying to tell you is that the session type as specified does not
/// correspond to a valid sequence of actions on the channel, because it contains unproductive
/// recursion.
pub trait Session
where
    Self: Scoped
        + Actionable<NextAction = <Self as Session>::Action>
        + HasDual<DualSession = <Self as Session>::Dual>,
{
    /// The dual to this session type, i.e. the session type for the other side of this channel.
    ///
    /// Every individual session type component has a dual defined by [`HasDual`]. This is that
    /// type.
    type Dual;

    /// The canonical next channel action for this session type.
    ///
    /// For [`Send`], [`Recv`], [`Offer`], [`Choose`], [`Split`], [`Seq`], and [`Done`] (when
    /// [`Done`] is outside a [`Loop`] or in the first argument to [`Seq`]), the next channel action
    /// is the session type itself. For [`Loop`], the next channel action is the inside of the loop,
    /// with all [`Continue`]s within it appropriately unrolled by one loop iteration.
    ///
    /// This is always the action type defined by [`Actionable`] for this session type.
    type Action;

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
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = Done::channel(mpsc::unbounded_channel);
    /// # }
    /// ```
    fn channel<Tx, Rx>(
        make: impl FnMut() -> (Tx, Rx),
    ) -> (Chan<Tx, Rx, Self>, Chan<Tx, Rx, <Self as Session>::Dual>)
    where
        <Self as Session>::Dual: Scoped + Actionable + HasDual,
        Tx: marker::Send + 'static,
        Rx: marker::Send + 'static;

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
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = Done::bichannel(
    ///     mpsc::unbounded_channel,
    ///     || mpsc::channel(1),
    /// );
    /// # }
    /// ```
    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self>,
        Chan<Tx1, Rx0, <Self as Session>::Dual>,
    )
    where
        <Self as Session>::Dual: Scoped + Actionable + HasDual,
        Tx0: marker::Send + 'static,
        Rx0: marker::Send + 'static,
        Tx1: marker::Send + 'static,
        Rx1: marker::Send + 'static;

    /// Given a transmitting and receiving end of an un-session-typed connection, wrap them in a new
    /// session-typed channel for the protocol `P.`
    ///
    /// It is expected that the other ends of these connections will be wrapped in a channel with
    /// the [`Dual`](crate::Session::Dual) session type.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let c = Done::wrap(tx, rx);
    /// c.close();
    /// # }
    /// ```
    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self>
    where
        Tx: marker::Send + 'static,
        Rx: marker::Send + 'static;

    /// Given a closure which runs the session on a channel from start to completion, run that
    /// session on the given pair of sending and receiving connections.
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
    /// # Examples
    ///
    /// ```
    /// use dialectic::prelude::*;
    /// use dialectic::backend::mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let (output, ends) = Done::over(tx, rx, |chan| async move {
    ///     chan.close();
    ///     Ok::<_, mpsc::Error>("Hello!".to_string())
    /// }).await?;
    ///
    /// assert_eq!(output, "Hello!");
    /// let (tx, rx) = ends?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// As noted above, an error is thrown when the channel is dropped too early:
    ///
    /// ```
    /// # use dialectic::prelude::*;
    /// # use dialectic::backend::mpsc;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use SessionIncomplete::BothHalves;
    /// use IncompleteHalf::Unfinished;
    ///
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let (_, ends) = <Send<String, Done>>::over(tx, rx, |chan| async move {
    ///     Ok::<_, mpsc::Error>(())
    /// }).await?;
    ///
    /// assert!(matches!(ends, Err(BothHalves { tx: Unfinished(_), rx: Unfinished(_) })));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// And likewise, an error is thrown when the channel is not dropped by the end of the future:
    ///
    /// ```
    /// # use dialectic::prelude::*;
    /// # use dialectic::backend::mpsc;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use std::sync::{Arc, Mutex};
    ///
    /// use SessionIncomplete::BothHalves;
    /// use IncompleteHalf::Unclosed;
    ///
    /// // We'll put the `Chan` here so it outlives the closure. **Don't do this!**
    /// let hold_on_to_chan = Arc::new(Mutex::new(None));
    /// let hold = hold_on_to_chan.clone();
    ///
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let (_, ends) = <Send<String, Done>>::over(tx, rx, |chan| async move {
    ///     *hold.lock().unwrap() = Some(chan);
    ///     Ok::<_, mpsc::Error>(())
    /// }).await?;
    ///
    /// assert!(matches!(ends, Err(BothHalves { tx: Unclosed, rx: Unclosed })));
    ///
    /// // Make sure the `Chan` outlives the closure. **Don't do this!**
    /// drop(hold_on_to_chan);
    /// # Ok(())
    /// # }
    /// ```
    fn over<Tx, Rx, T, Err, F, Fut>(tx: Tx, rx: Rx, with_chan: F) -> Over<Tx, Rx, T, Err, Fut>
    where
        Tx: std::marker::Send + 'static,
        Rx: std::marker::Send + 'static,
        F: FnOnce(Chan<Tx, Rx, Self>) -> Fut,
        Fut: Future<Output = Result<T, Err>>;
}

impl<P> Session for P
where
    Self: Scoped + Actionable + HasDual,
{
    type Dual = <Self as HasDual>::DualSession;
    type Action = <Self as Actionable>::NextAction;

    fn channel<Tx, Rx>(
        mut make: impl FnMut() -> (Tx, Rx),
    ) -> (Chan<Tx, Rx, Self>, Chan<Tx, Rx, <Self as Session>::Dual>)
    where
        <Self as Session>::Dual: Scoped + Actionable + HasDual,
        Tx: marker::Send + 'static,
        Rx: marker::Send + 'static,
    {
        let (tx0, rx0) = make();
        let (tx1, rx1) = make();
        (P::wrap(tx0, rx1), <Self::Dual>::wrap(tx1, rx0))
    }

    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self>,
        Chan<Tx1, Rx0, <Self as Session>::Dual>,
    )
    where
        <Self as Session>::Dual: Scoped + Actionable + HasDual,
        Tx0: marker::Send + 'static,
        Rx0: marker::Send + 'static,
        Tx1: marker::Send + 'static,
        Rx1: marker::Send + 'static,
    {
        let (tx0, rx0) = make0();
        let (tx1, rx1) = make1();
        (P::wrap(tx0, rx1), <Self::Dual>::wrap(tx1, rx0))
    }

    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self>
    where
        Tx: marker::Send + 'static,
        Rx: marker::Send + 'static,
    {
        chan::Chan::from_raw_unchecked(tx, rx)
    }

    fn over<Tx, Rx, T, Err, F, Fut>(tx: Tx, rx: Rx, with_chan: F) -> Over<Tx, Rx, T, Err, Fut>
    where
        Tx: std::marker::Send + 'static,
        Rx: std::marker::Send + 'static,
        F: FnOnce(Chan<Tx, Rx, Self>) -> Fut,
        Fut: Future<Output = Result<T, Err>>,
    {
        crate::chan::over::<Self, _, _, _, _, _, _>(tx, rx, with_chan)
    }
}
