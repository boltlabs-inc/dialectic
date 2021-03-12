pub use crate::chan::Over;

use super::*;
use crate::types::*;
use std::marker;

/// The `Session` extension trait gives methods to create session-typed channels from session types.
/// These are implemented as static methods on the session type itself.
///
/// This trait is already implemented for all valid session types, and cannot be extended by users
/// of this crate.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic_tokio_mpsc as mpsc;
///
/// let (c1, c2) = <Session! { send String }>::channel(mpsc::unbounded_channel);
/// // do something with these channels...
/// ```
///
/// # Counterexamples
///
/// It is only possible to create a session-typed channel when the session type is valid. The
/// following examples fail, for the reasons described:
///
/// 1. The session type `send &'a str` for a non-static `'a` is not `'static`, but all
///    session types must be `'static`:
///
///    ```compile_fail
///    # use dialectic::prelude::*;
///    # use dialectic_tokio_mpsc as mpsc;
///    fn something<'a>(_: &'a str) {
///        let (c1, c2) = <Session! { send &'a str }>::channel(mpsc::unbounded_channel);
///    }
///    ```
///
/// 2. The session type `Loop<Continue<_1>>` is not `Scoped`, because `Continue<_1>` must occur
///    within two nested [`Loop`]s to be properly scoped:
///
///    ```compile_fail
///    # use dialectic::prelude::*;
///    # use dialectic_tokio_mpsc as mpsc;
///    use dialectic::types::{Loop, Continue};
///    use dialectic::unary::types::*;
///    let (c1, c2) = <Loop<Continue<_1>>>::channel(mpsc::unbounded_channel);
///    ```
///
///    Note that you cannot write out an ill-scoped session type using the
///    [`Session!`](crate::Session@macro) macro, because it will throw an error if you try.
///
/// 3. The session type `loop {}` is not `Actionable` because it is an "unproductive"
///    infinite loop, where no matter how many times you loop, there will never be an available
///    action to perform on the channel:
///
///    ```compile_fail
///    # use dialectic::prelude::*;
///    # use dialectic_tokio_mpsc as mpsc;
///    let (c1, c2) = <Session! { loop {} }>::channel(mpsc::unbounded_channel);
///    ```
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
    /// For [`Send`], [`Recv`], [`Offer`], [`Choose`], [`Split`], [`Call`], and [`Done`], the next
    /// channel action is the session type itself. For [`Loop`], the next channel action is the
    /// inside of the loop, with all [`Continue`]s within it appropriately unrolled by one loop
    /// iteration.
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
    /// use dialectic_tokio_mpsc as mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = <Session! { /* ... */ }>::channel(mpsc::unbounded_channel);
    /// # }
    /// ```
    fn channel<Tx, Rx>(
        make: impl FnMut() -> (Tx, Rx),
    ) -> (Chan<Self, Tx, Rx>, Chan<Self::Dual, Tx, Rx>)
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
    /// use dialectic_tokio_mpsc as mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = <Session! { /* ... */ }>::bichannel(
    ///     mpsc::unbounded_channel,
    ///     || mpsc::channel(1),
    /// );
    /// # }
    /// ```
    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (Chan<Self, Tx0, Rx1>, Chan<Self::Dual, Tx1, Rx0>)
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
    /// use dialectic_tokio_mpsc as mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let c = <Session! { /* ... */ }>::wrap(tx, rx);
    /// c.close();
    /// # }
    /// ```
    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Self, Tx, Rx>
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
    /// use dialectic_tokio_mpsc as mpsc;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let (output, ends) = <Session! { /* ... */ }>::over(tx, rx, |chan| async move {
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
    /// # use dialectic_tokio_mpsc as mpsc;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use dialectic::SessionIncomplete::BothHalves;
    /// use dialectic::IncompleteHalf::Unfinished;
    ///
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let (_, ends) = <Session! { send String }>::over(tx, rx, |chan| async move {
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
    /// # use dialectic_tokio_mpsc as mpsc;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use std::sync::{Arc, Mutex};
    ///
    /// use dialectic::SessionIncomplete::BothHalves;
    /// use dialectic::IncompleteHalf::Unclosed;
    ///
    /// // We'll put the `Chan` here so it outlives the closure. **Don't do this!**
    /// let hold_on_to_chan = Arc::new(Mutex::new(None));
    /// let hold = hold_on_to_chan.clone();
    ///
    /// let (tx, rx) = mpsc::unbounded_channel();
    /// let (_, ends) = <Session! { send String }>::over(tx, rx, |chan| async move {
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
        F: FnOnce(Chan<Self, Tx, Rx>) -> Fut,
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
    ) -> (Chan<Self, Tx, Rx>, Chan<Self::Dual, Tx, Rx>)
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
    ) -> (Chan<Self, Tx0, Rx1>, Chan<Self::Dual, Tx1, Rx0>)
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

    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Self, Tx, Rx>
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
        F: FnOnce(Chan<Self, Tx, Rx>) -> Fut,
        Fut: Future<Output = Result<T, Err>>,
    {
        crate::chan::over::<Self, _, _, _, _, _, _>(tx, rx, with_chan)
    }
}
