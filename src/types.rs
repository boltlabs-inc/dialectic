//! The types in this module enumerate the shapes of all expressible sessions.

use super::*;
pub use unary::*;

/// A session type describes the sequence of operations performed by one end of a bidirectional
/// [`Chan`]. Each session type has a [`Session::Dual`], the type of the corresponding client on the
/// other side of the channel. The sealed trait `Session` enumerates these types, and provides the
/// dual of each.
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
{
    /// The next actual channel action: [`Send`], [`Recv`], [`Offer`], [`Choose`], or [`Split`].
    /// This steps through [`Loop`] and [`Recur`] transparently.
    type Action: Session;

    /// The environment resulting from stepping through one or many [`Loop`] or [`Recur`] points to
    /// the next real channel action.
    type Env: Environment;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols is [`Session`].
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

impl<P: Session, Ps: EachSession> EachSession for (P, Ps) {
    type Dual = (P::Dual, Ps::Dual);
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols is [`Actionable`].
pub trait EachActionable<E: Environment>: EachSession
where
    E::Dual: Environment,
{
}
impl<E: Environment> EachActionable<E> for () where E::Dual: Environment {}
impl<E: Environment, P: Actionable<E>, Ps: EachActionable<E>> EachActionable<E> for (P, Ps)
where
    P::Dual: Actionable<E::Dual>,
    <<P::Dual as Actionable<<E>::Dual>>::Env as types::EachSession>::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    E::Dual: Environment,
{
}

/// A valid session environment is a type-level list of session types, each of which may refer by
/// [`Recur`] index to any other session in the list which is *below or including* itself.
pub trait Environment: EachSession
where
    Self::Dual: Environment,
{
    type Depth: Unary;
}

impl Environment for () {
    type Depth = Z;
}

impl<P: Session, Ps: Environment> Environment for (P, Ps)
where
    P: Scoped<Ps::Depth>,
    P: Scoped<<Ps::Dual as Environment>::Depth>,
    P::Dual: Scoped<Ps::Depth>,
    P::Dual: Scoped<<Ps::Dual as Environment>::Depth>,
    Ps::Dual: Environment,
{
    type Depth = S<Ps::Depth>;
}

/// A session type is scoped for a given environment depth `N` if it [`Recur`]s no more than `N`
/// [`Loop`] levels above itself.
pub trait Scoped<N: Unary>: Session {}
impl<T: 'static, P: Scoped<N>, N: Unary> Scoped<N> for Recv<T, P> {}
impl<T: 'static, P: Scoped<N>, N: Unary> Scoped<N> for Send<T, P> {}
impl<Ps: EachScoped<N>, N: Unary> Scoped<N> for Offer<Ps> {}
impl<Ps: EachScoped<N>, N: Unary> Scoped<N> for Choose<Ps> {}
impl<P: Scoped<S<N>>, N: Unary> Scoped<N> for Loop<P> {}
impl<N: Unary> Scoped<N> for Recur<Z> {}
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

pub mod unary {
    //! The unary numbers, represented by zero [`Z`] and successor [`S`].

    /// The number zero.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Z;

    /// The successor of `N` (i.e. `N + 1`).
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct S<N>(N);

    /// All unary numbers can be converted to their value-level equivalent `usize`.
    pub trait Unary: sealed::Sealed + 'static {
        const VALUE: usize;
    }

    impl Unary for Z {
        const VALUE: usize = 0;
    }

    impl<N: Unary> Unary for S<N> {
        const VALUE: usize = N::VALUE + 1;
    }

    mod sealed {
        use super::*;
        pub trait Sealed: 'static {}
        impl Sealed for Z {}
        impl<N: Sealed> Sealed for S<N> {}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Complete a session. The only thing to do with a [`Chan`] at its `End` is to drop it.
pub struct End;

impl Session for End {
    type Dual = End;
}

impl<E: Environment> Actionable<E> for End
where
    E::Dual: Environment,
{
    type Action = End;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Receive a message of type `T` using [`Chan::recv`], then continue with protocol `P`.
pub struct Recv<T: 'static, P>(pub PhantomData<T>, pub P);

impl<T, P: Session> Session for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

impl<E: Environment, T, P: Session> Actionable<E> for Recv<T, P>
where
    E::Dual: Environment,
{
    type Action = Recv<T, P>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Send a message of type `T` using [`Chan::send`], then continue with protocol `P`.
pub struct Send<T: 'static, P>(pub PhantomData<T>, pub P);

impl<T, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

impl<E: Environment, T, P: Session> Actionable<E> for Send<T, P>
where
    E::Dual: Environment,
{
    type Action = Send<T, P>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Actively choose using [`Chan::choose`] between any of the protocols in the tuple `Ps`.
pub struct Choose<Ps>(pub Ps);

impl<Ps: EachSession> Session for Choose<Ps> {
    type Dual = Offer<Ps::Dual>;
}

impl<E: Environment, Ps: EachSession> Actionable<E> for Choose<Ps>
where
    E::Dual: Environment,
{
    type Action = Choose<Ps>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Offer the choice using [`Chan::offer`] or the `offer!` macro between any of the protocols in the
/// tuple `Ps`.
pub struct Offer<Ps>(pub Ps);

impl<Ps: EachSession> Session for Offer<Ps> {
    type Dual = Choose<Ps::Dual>;
}

impl<E: Environment, Ps: EachSession> Actionable<E> for Offer<Ps>
where
    E::Dual: Environment,
{
    type Action = Offer<Ps>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Split the connection into send-only and receive-only halves using [`Chan::split`]. These can
/// subsequently be rejoined using [`Chan::unsplit`].
pub struct Split<P, Q>(pub P, pub Q);

impl<P: Session, Q: Session> Session for Split<P, Q> {
    // Note how the dual flips the position of P and Q, because P::Dual is a receiving session, and
    // therefore belongs on the right of the split, and Q::Dual is a sending session, and therefore
    // belongs on the left of the split.
    type Dual = Split<Q::Dual, P::Dual>;
}

impl<E: Environment, P: Session, Q: Session> Actionable<E> for Split<P, Q>
where
    E::Dual: Environment,
{
    type Action = Split<P, Q>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Label a loop point, which can be reiterated with [`Recur`].
pub struct Loop<P>(pub P);

impl<P: Session> Session for Loop<P> {
    type Dual = Loop<P::Dual>;
}

impl<P, E> Actionable<E> for Loop<P>
where
    E: Environment,
    E::Dual: Environment,
    P: Actionable<(P, E)>,
    P: Scoped<E::Depth>,
    P: Scoped<<E::Dual as Environment>::Depth>,
    P::Dual: Scoped<E::Depth>,
    P::Dual: Scoped<<E::Dual as Environment>::Depth>,
    <P::Env as EachSession>::Dual: Environment,
{
    type Action = <P as Actionable<(P, E)>>::Action;
    type Env = <P as Actionable<(P, E)>>::Env;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Repeat a loop, which infers from the number in the type which loop head to recur to.
pub struct Recur<N = Z>(pub N);

impl<N: Unary> Session for Recur<N> {
    type Dual = Recur<N>;
}

impl<E: Select<N> + Environment, N: Unary> Actionable<E> for Recur<N>
where
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
