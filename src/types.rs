//! The types in this module enumerate the shapes of all expressible sessions.

use super::*;
pub use unary::*;

/// A session type describes the sequence of operations performed by one end of a bidirectional
/// [`Chan`]. Each session type has a [`Session::Dual`], the type of the corresponding client on the
/// other side of the channel. The sealed trait `Session` enumerates these types, and provides the
/// dual of each.
pub trait Session: Any + sealed::Session {
    /// The dual to this session type, i.e. the session type required of the other end of the
    /// channel.
    type Dual;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed `AllSession` trait ensures that every protocol in a type level list of
/// protocols is `Session`.
pub trait AllSession: sealed::AllSession {
    type AllDual;
    type Arity: Unary;
}

impl AllSession for () {
    type AllDual = ();
    type Arity = Z;
}

impl<P: Session, Ps: AllSession> AllSession for (P, Ps) {
    type AllDual = (P::Dual, Ps::AllDual);
    type Arity = S<Ps::Arity>;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Receive a message of type `T` using [`Chan::recv`], then continue with protocol `P`.
pub struct Recv<T, P>(pub PhantomData<T>, pub P);

impl<T: marker::Send + Any, P: Session> Session for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Send a message of type `T` using [`Chan::send`], then continue with protocol `P`.
pub struct Send<T, P>(pub PhantomData<T>, pub P);

impl<T: marker::Send + Any, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Actively choose using [`Chan::choose`] between any of the protocols in the tuple `Ps`.
pub struct Choose<Ps>(pub Ps);

impl<Ps: AllSession> Session for Choose<Ps> {
    type Dual = Offer<Ps::AllDual>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Offer the choice using [`Chan::offer`] or the `offer!` macro between any of the protocols in the
/// tuple `Ps`.
pub struct Offer<Ps>(pub Ps);

impl<Ps: AllSession> Session for Offer<Ps> {
    type Dual = Choose<Ps::AllDual>;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Label a loop point using [`Chan::enter`], which can be reiterated with [`Recur`].
pub struct Loop<P>(pub P);

impl<P: Session> Session for Loop<P> {
    type Dual = Loop<P::Dual>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// Repeat a loop using [`Chan::recur`], which infers from the number in the type which loop head to
/// recur to.
pub struct Recur<N = Z>(pub N);

impl<N: marker::Send + Any> Session for Recur<N> {
    type Dual = Recur<N>;
}
