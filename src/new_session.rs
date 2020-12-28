use super::*;

/// The `NewSession` extension trait gives methods to create session-typed channels from session
/// types. These are implemented as static methods on the session type itself.
///
/// # Examplees
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
/// [`Continue`] directly inside the loop to which that recursion refers. For instance, attempting
/// to use the session type `Loop<Continue>` will result in a compile-time trait solver overflow.
///
/// ```compile_fail
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Loop<Continue>>::channel(backend::mpsc::unbounded_channel);
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
///   = note: required because of the requirements on the impl of `NewSession` for `Loop<Continue>`
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
    Self: Actionable,
    Self::Dual: Actionable,
    <Self::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable>::Env as EachSession>::Dual: Environment,
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
    /// let (c1, c2) = Done::channel(backend::mpsc::unbounded_channel);
    /// # }
    /// ```
    fn channel<Tx, Rx>(
        make: impl FnMut() -> (Tx, Rx),
    ) -> (
        Chan<Tx, Rx, Self::Action, Self::Env>,
        Chan<Tx, Rx, <Self::Dual as Actionable>::Action, <Self::Dual as Actionable>::Env>,
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
    /// let (c1, c2) = Done::bichannel(
    ///     backend::mpsc::unbounded_channel,
    ///     || backend::mpsc::channel(1),
    /// );
    /// # }
    /// ```
    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self::Action, Self::Env>,
        Chan<Tx1, Rx0, <Self::Dual as Actionable>::Action, <Self::Dual as Actionable>::Env>,
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
    /// let c = Done::wrap(&mut tx, &mut rx);  // you can wrap &mut references
    /// c.close();                            // whose lifetimes end when the channel is closed,
    /// let c = Done::wrap(tx, rx);            // or you can wrap owned values
    /// let (tx, rx) = c.close();             // and get them back when the channel is closed.
    /// # }
    /// ```
    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self::Action, Self::Env>;
}

impl<P> NewSession for P
where
    Self: Actionable,
    Self::Dual: Actionable,
    <Self::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable>::Env as EachSession>::Dual: Environment,
{
    fn channel<Tx, Rx>(
        mut make: impl FnMut() -> (Tx, Rx),
    ) -> (
        Chan<Tx, Rx, Self::Action, Self::Env>,
        Chan<Tx, Rx, <Self::Dual as Actionable>::Action, <Self::Dual as Actionable>::Env>,
    ) {
        let (tx0, rx0) = make();
        let (tx1, rx1) = make();
        (P::wrap(tx0, rx1), <P::Dual>::wrap(tx1, rx0))
    }

    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self::Action, Self::Env>,
        Chan<Tx1, Rx0, <Self::Dual as Actionable>::Action, <Self::Dual as Actionable>::Env>,
    ) {
        let (tx0, rx0) = make0();
        let (tx1, rx1) = make1();
        (P::wrap(tx0, rx1), <P::Dual>::wrap(tx1, rx0))
    }

    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self::Action, Self::Env> {
        unsafe { Chan::with_env(tx, rx) }
    }
}
