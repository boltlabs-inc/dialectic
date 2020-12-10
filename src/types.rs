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
pub trait Actionable<E>: Session {
    /// The next actual channel action: [`Send`], [`Recv`], [`Offer`], [`Choose`], or [`Split`].
    /// This steps through [`Loop`] and [`Recur`] transparently.
    type Action;

    /// The environment resulting from stepping through one or many [`Loop`] or [`Recur`] points to
    /// the next real channel action.
    type Env;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols is [`Session`].
pub trait EachSession: Sized + sealed::EachSession
where
    Self::EachDual: EachSession<EachDual = Self>,
{
    /// The point-wise [`Session::Dual`] of a type-level list of session types.
    type EachDual;
}

impl EachSession for () {
    type EachDual = ();
}

impl<P: Session, Ps: EachSession> EachSession for (P, Ps) {
    type EachDual = (P::Dual, Ps::EachDual);
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols is [`Actionable`].
pub trait EachActionable<E>
where
    Self: EachSession,
{
}

impl<E> EachActionable<E> for () {}

impl<E: EachSession, P: Actionable<E>, Ps: EachActionable<E>> EachActionable<E> for (P, Ps) where
    P::Dual: Actionable<E::EachDual>
{
}

/// A valid session environment is a type-level list of session types, each of which may refer by
/// [`Recur`] index to any other session in the list which is *below or including* itself.
///
/// Notes
///
/// This trait does not ensure that the environment is fully valid; rather, it ensures that every
/// session in the environment is [`Actionable`], which means it can take at least one productive
/// step forward.
pub trait Environment: EachSession
where
    Self::EachDual: Environment,
{
}

impl Environment for () {}

impl<P: Actionable<(P, Ps)>, Ps: Environment> Environment for (P, Ps)
where
    P::Dual: Actionable<(P::Dual, Ps::EachDual)>,
    Ps::EachDual: Environment,
{
}

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
    pub trait Unary: sealed::Sealed {
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
        pub trait Sealed {}
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

impl<E: EachSession> Actionable<E> for End {
    type Action = End;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Receive a message of type `T` using [`Chan::recv`], then continue with protocol `P`.
pub struct Recv<T, P>(pub PhantomData<T>, pub P);

impl<T: Any, P: Session> Session for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

impl<E: EachSession, T: 'static, P: Session> Actionable<E> for Recv<T, P> {
    type Action = Recv<T, P>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Send a message of type `T` using [`Chan::send`], then continue with protocol `P`.
pub struct Send<T, P>(pub PhantomData<T>, pub P);

impl<T: Any, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

impl<E: EachSession, T: 'static, P: Session> Actionable<E> for Send<T, P> {
    type Action = Send<T, P>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Actively choose using [`Chan::choose`] between any of the protocols in the tuple `Ps`.
pub struct Choose<Ps>(pub Ps);

impl<Ps: EachSession> Session for Choose<Ps> {
    type Dual = Offer<Ps::EachDual>;
}

impl<E: EachSession, Ps: EachSession> Actionable<E> for Choose<Ps> {
    type Action = Choose<Ps>;
    type Env = E;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Offer the choice using [`Chan::offer`] or the `offer!` macro between any of the protocols in the
/// tuple `Ps`.
pub struct Offer<Ps>(pub Ps);

impl<Ps: EachSession> Session for Offer<Ps> {
    type Dual = Choose<Ps::EachDual>;
}

impl<E: EachSession, Ps: EachSession> Actionable<E> for Offer<Ps> {
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

impl<E: EachSession, P: Session, Q: Session> Actionable<E> for Split<P, Q> {
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
    E: EachSession,
    P: Actionable<(P, E)>,
{
    type Action = <P as Actionable<(P, E)>>::Action;
    type Env = <P as Actionable<(P, E)>>::Env;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Repeat a loop, which infers from the number in the type which loop head to recur to.
pub struct Recur<N = Z>(pub N);

impl<N: marker::Send + Any> Session for Recur<N> {
    type Dual = Recur<N>;
}

impl<E: Select<N> + EachSession, N: marker::Send + Any> Actionable<E> for Recur<N>
where
    E::Selected: Actionable<E>,
    <E::Selected as Actionable<E>>::Action: Actionable<E>,
{
    type Action = <E::Selected as Actionable<E>>::Action;
    type Env = <E::Selected as Actionable<E>>::Env;
}
