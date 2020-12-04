//! The types in this module enumerate the shapes of all expressible sessions.

use super::*;
pub use unary::*;

pub mod unary {
    //! The unary numbers, represented by zero [`Z`] and successor [`S`].

    #[derive(Debug, Clone, Copy)]
    pub struct Z;

    #[derive(Debug, Clone, Copy)]
    pub struct S<N>(N);
}

#[derive(Debug, Clone, Copy)]
/// Complete a session. The only thing to do with a [`Chan`] at its `End` is to drop it.
pub struct End;

impl Session for End {
    type Dual = End;
}

#[derive(Debug, Clone, Copy)]
/// Receive a message of type `T` using [`Chan::recv`], then continue with protocol `P`.
pub struct Recv<T, P>(pub PhantomData<T>, pub P);

impl<T: marker::Send + Any, P: Session> Session for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

#[derive(Debug, Clone, Copy)]
/// Send a message of type `T` using [`Chan::send`], then continue with protocol `P`.
pub struct Send<T, P>(pub PhantomData<T>, pub P);

impl<T: marker::Send + Any, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

#[derive(Debug, Clone, Copy)]
/// Actively choose using [`Chan::choose`] between any of the protocols in the tuple `Ps`.
pub struct Choose<Ps>(pub Ps);

impl<Ps: AllSession> Session for Choose<Ps> {
    type Dual = Offer<Ps::AllDual>;
}

#[derive(Debug, Clone, Copy)]
/// Offer the choice using [`Chan::offer`] or the `offer!` macro between any of the protocols in the
/// tuple `Ps`.
pub struct Offer<Ps>(pub Ps);

impl<Ps: AllSession> Session for Offer<Ps> {
    type Dual = Choose<Ps::AllDual>;
}

#[derive(Debug, Clone, Copy)]
/// Split the connection into send-only and receive-only halves using [`Chan::split`]. These can
/// subsequently be rejoined using [`Chan::unsplit`].
pub struct Split<P, Q>(pub P, pub Q);

impl<P: Session, Q: Session> Session for Split<P, Q> {
    // Note how the dual flips the position of P and Q, because P::Dual is a receiving session, and
    // therefore belongs on the right of the split, and Q::Dual is a sending session, and therefore
    // belongs on the left of the split.
    type Dual = Split<Q::Dual, P::Dual>;
}

#[derive(Debug, Clone, Copy)]
/// Label a loop point using [`Chan::enter`], which can be reiterated with [`Recur`].
pub struct Loop<P>(pub P);

impl<P: Session> Session for Loop<P> {
    type Dual = Loop<P::Dual>;
}

#[derive(Debug, Clone, Copy)]
/// Repeat a loop using [`Chan::recur`], which infers from the number in the type which loop head to
/// recur to.
pub struct Recur<N = Z>(pub N);

impl<N: marker::Send + Any> Session for Recur<N> {
    type Dual = Recur<N>;
}
