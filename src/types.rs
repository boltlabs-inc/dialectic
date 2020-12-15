//! The types in this module enumerate the shapes of all expressible sessions.

use super::*;
pub use unary::*;

pub mod tuple;
pub mod unary;

/// A session type describes the sequence of operations performed by one end of a bidirectional
/// [`Chan`]. Each session type has a [`Session::Dual`], the type of the corresponding client on
/// the other side of the channel. The sealed trait `Session` enumerates these types, and provides
/// the dual of each.
pub trait Session: Sized + sealed::IsSession {
    /// The dual to this session type, i.e. the session type required of the other end of the
    /// channel.
    type Dual: Session<Dual = Self>;
}

/// The [`Actionable`] trait infers the next action necessary on a channel, automatically stepping
/// through [`Loop`]s and [`Recur`]sion points.
pub trait Actionable<E>: Session
where
    E: Environment,
    E::Dual: Environment,
    <Self::Env as EachSession>::Dual: Environment,
    Self: Scoped<E::Depth>,
{
    /// The next actual channel action: [`Send`], [`Recv`], [`Offer`], [`Choose`], or [`Split`].
    /// This steps through [`Loop`] and [`Recur`] transparently.
    ///
    /// The constraints on this associated type ensure that it is idemopotent: the `Action` and
    /// `Env` of an `Action` are the same as those of that `Action`.
    type Action: Actionable<Self::Env, Action = Self::Action, Env = Self::Env>;

    /// The environment resulting from stepping through one or many [`Loop`] or [`Recur`] points to
    /// the next real channel action.
    type Env: Environment;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols [`Session`].
pub trait EachSession: Sized + sealed::EachSession
where
    Self::Dual: EachSession<Dual = Self>,
{
    /// The point-wise [`Session::Dual`] of a type-level list of session types.
    type Dual;
}

impl EachSession for () {
    type Dual = ();
}

impl<P, Ps> EachSession for (P, Ps)
where
    P: Session,
    Ps: EachSession,
{
    type Dual = (P::Dual, Ps::Dual);
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols is [`Actionable`].
pub trait EachActionable<E>: EachSession
where
    E: Environment,
    E::Dual: Environment,
{
}

impl<E> EachActionable<E> for ()
where
    E: Environment,
    E::Dual: Environment,
{
}

impl<E, P, Ps> EachActionable<E> for (P, Ps)
where
    P: Actionable<E>,
    Ps: EachActionable<E>,
    P::Dual: Actionable<E::Dual>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <<P::Dual as Actionable<<E>::Dual>>::Env as EachSession>::Dual: Environment,
{
}

/// A valid session environment is a type-level list of session types, each of which may refer by
/// [`Recur`] index to any other session in the list which is *below or including* itself.
pub trait Environment: EachSession
where
    Self::Dual: Environment,
{
    /// The depth of a session environment is the number of loops to which a [`Recur`] could jump,
    /// i.e. the number of session types in the session environment.
    type Depth: Unary;
}

impl Environment for () {
    type Depth = Z;
}

impl<P, Ps> Environment for (P, Ps)
where
    P: Scoped<S<Ps::Depth>>,
    P: Scoped<S<<Ps::Dual as Environment>::Depth>>,
    P::Dual: Scoped<S<Ps::Depth>>,
    P::Dual: Scoped<S<<Ps::Dual as Environment>::Depth>>,
    Ps: Environment,
    Ps::Dual: Environment,
{
    type Depth = S<Ps::Depth>;
}

/// A session type is scoped for a given environment depth `N` if it [`Recur`]s no more than `N`
/// [`Loop`] levels above itself.
///
/// A session type is `Scoped<Z>` (which can be abbreviated `Scoped`) if it does not [`Recur`] to
/// any loop above itself, i.e. all `Recur<N>` refer to a loop which they themselves are within.
pub trait Scoped<N: Unary = Z>: Session {}
impl<N: Unary, T, P: Scoped<N>> Scoped<N> for Recv<T, P> {}
impl<N: Unary, T, P: Scoped<N>> Scoped<N> for Send<T, P> {}
impl<N: Unary, Choices: Tuple> Scoped<N> for Offer<Choices>
where
    Choices::AsList: EachScoped<N>,
    <Choices::AsList as EachSession>::Dual: List,
{
}
impl<N: Unary, Choices: Tuple> Scoped<N> for Choose<Choices>
where
    Choices::AsList: EachScoped<N>,
    <Choices::AsList as EachSession>::Dual: List,
{
}
impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Split<P, Q> {}
impl<N: Unary, P: Scoped<S<N>>> Scoped<N> for Loop<P> {}
impl<N: Unary> Scoped<S<N>> for Recur<Z> {}
impl<N: Unary> Scoped<S<N>> for Recur<S<N>> where Recur<N>: Scoped<N> {}
impl<N: Unary> Scoped<N> for End {}

/// In the [`Choose`] and [`Offer`] session types, `EachScoped<N>` is used to assert that every
/// choice or offering is well-[`Scoped`].
pub trait EachScoped<N: Unary>: EachSession {}
impl<N: Unary> EachScoped<N> for () {}
impl<N: Unary, P: Scoped<N>, Ps: EachScoped<N>> EachScoped<N> for (P, Ps) {}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed `Select` trait describes what it means to index into a type level list of
/// protocols.
pub trait Select<N: Unary>: sealed::Select<N> {
    /// The thing which is selected from this list by the index `N`.
    type Selected;
}

impl<T, S> Select<Z> for (T, S) {
    type Selected = T;
}

impl<T, P, N: Unary> Select<S<N>> for (T, (P, ()))
where
    (P, ()): Select<N>,
{
    type Selected = <(P, ()) as Select<N>>::Selected;
}

/// Complete a session. The only thing to do with a [`Chan`] at its `End` is to drop it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct End;

impl Session for End {
    type Dual = End;
}

impl<E> Actionable<E> for End
where
    E: Environment,
    E::Dual: Environment,
{
    type Action = End;
    type Env = E;
}

/// Receive a message of type `T` using [`Chan::recv`], then continue with protocol `P`.
///
/// # Notes
///
/// A session ending with a `Recv` can be abbreviated: `Recv<String>` is shorthand for `Recv<String,
/// End>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Recv<T, P = End>(pub PhantomData<T>, pub P);

impl<T, P: Session> Session for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

impl<E, T, P> Actionable<E> for Recv<T, P>
where
    P: Scoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Recv<T, P>;
    type Env = E;
}

/// Send a message of type `T` using [`Chan::send`], then continue with protocol `P`.
///
/// # Notes
///
/// A session ending with a `Send` can be abbreviated: `Send<String>` is shorthand for `Send<String,
/// End>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Send<T, P = End>(pub PhantomData<T>, pub P);

impl<T, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

impl<E, T, P> Actionable<E> for Send<T, P>
where
    P: Scoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Send<T, P>;
    type Env = E;
}

/// Actively choose using [`Chan::choose`] between any of the protocols in the tuple `Choices`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Choose<Choices>(pub Choices);

impl<Choices> Session for Choose<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
{
    type Dual = Offer<<<Choices::AsList as EachSession>::Dual as List>::AsTuple>;
}

impl<E, Choices> Actionable<E> for Choose<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
    Choices::AsList: EachScoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Choose<Choices>;
    type Env = E;
}

/// Offer the choice using [`Chan::offer`] or the `offer!` macro between any of the protocols in the
/// tuple `Choices`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Offer<Choices>(pub Choices);

impl<Choices> Session for Offer<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
{
    type Dual = Choose<<<Choices::AsList as EachSession>::Dual as List>::AsTuple>;
}

impl<E, Choices> Actionable<E> for Offer<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
    Choices::AsList: EachScoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Offer<Choices>;
    type Env = E;
}

/// Split the connection into send-only and receive-only halves using [`Chan::split`]. These can
/// subsequently be rejoined using [`Chan::unsplit`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Split<P, Q>(pub P, pub Q);

impl<P: Session, Q: Session> Session for Split<P, Q> {
    // Note how the dual flips the position of P and Q, because P::Dual is a receiving session, and
    // therefore belongs on the right of the split, and Q::Dual is a sending session, and therefore
    // belongs on the left of the split.
    type Dual = Split<Q::Dual, P::Dual>;
}

impl<E, P, Q> Actionable<E> for Split<P, Q>
where
    P: Scoped<E::Depth>,
    Q: Scoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Split<P, Q>;
    type Env = E;
}

/// Label a loop point, which can be reiterated with [`Recur`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Loop<P>(pub P);

impl<P: Session> Session for Loop<P> {
    type Dual = Loop<P::Dual>;
}

impl<P, E> Actionable<E> for Loop<P>
where
    E: Environment,
    E::Dual: Environment,
    P: Actionable<(P, E)>,
    P: Scoped<S<E::Depth>>,
    P: Scoped<S<<E::Dual as Environment>::Depth>>,
    P::Dual: Scoped<S<E::Depth>>,
    P::Dual: Scoped<S<<E::Dual as Environment>::Depth>>,
    <P::Env as EachSession>::Dual: Environment,
{
    type Action = <P as Actionable<(P, E)>>::Action;
    type Env = <P as Actionable<(P, E)>>::Env;
}

/// Repeat a loop, which infers from the number in the type which loop head to recur to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Recur<N: Unary = Z>(pub N);

impl<N: Unary> Session for Recur<N> {
    type Dual = Recur<N>;
}

impl<E, N: Unary> Actionable<E> for Recur<N>
where
    E: Select<N> + Environment,
    Recur<N>: Scoped<E::Depth>,
    E::Selected: Actionable<E>,
    <E::Selected as Actionable<E>>::Action: Actionable<E>,
    <<E::Selected as Actionable<E>>::Env as EachSession>::Dual: Environment,
    <<<E::Selected as Actionable<E>>::Action as Actionable<E>>::Env as EachSession>::Dual:
        Environment,
    E::Dual: Environment,
{
    type Action = <E::Selected as Actionable<E>>::Action;
    type Env = <E::Selected as Actionable<E>>::Env;
}

mod sealed {
    use super::*;

    pub trait IsSession {}
    impl IsSession for End {}
    impl<T, P> IsSession for Recv<T, P> {}
    impl<T, P> IsSession for Send<T, P> {}
    impl<Choices> IsSession for Choose<Choices> {}
    impl<Choices> IsSession for Offer<Choices> {}
    impl<P, Q> IsSession for Split<P, Q> {}
    impl<P> IsSession for Loop<P> {}
    impl<N: Unary> IsSession for Recur<N> {}

    pub trait EachSession {}
    impl EachSession for () {}
    impl<T: IsSession, Ts: EachSession> EachSession for (T, Ts) {}

    pub trait Select<N: Unary> {}
    impl<T, S> Select<Z> for (T, S) {}
    impl<T, P, N: Unary> Select<S<N>> for (T, (P, ())) where (P, ()): Select<N> {}
}
