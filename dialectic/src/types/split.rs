use std::{any::Any, marker::PhantomData};

use super::sealed::IsSession;
use super::*;

/// Split the connection into send-only and receive-only halves using [`split`](crate::Chan::split).
///
/// The type `Split<P, Q, R>` means: do the [`Transmit`](crate::backend::Transmit)-only session `P`
/// concurrently with the [`Receive`](crate::backend::Receive)-only session `Q`, running them both
/// to [`Done`], and when they've completed, do the session `R`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Split<P, Q, R>(
    PhantomData<fn() -> P>,
    PhantomData<fn() -> Q>,
    PhantomData<fn() -> R>,
);

impl<P, Q, R> Default for Split<P, Q, R> {
    fn default() -> Self {
        Split(PhantomData, PhantomData, PhantomData)
    }
}

impl<P: Any, Q: Any, R: Any> IsSession for Split<P, Q, R> {}

impl<P: HasDual, Q: HasDual, R: HasDual> HasDual for Split<P, Q, R> {
    /// Note how the dual flips the position of `P` and `Q`, because `P::Dual` is a receiving
    /// session, and therefore belongs on the right of the split, and `Q::Dual` is a sending
    /// session, and therefore belongs on the left of the split.
    type DualSession = Split<Q::DualSession, P::DualSession, R::DualSession>;
}

impl<P: 'static, Q: 'static, R: 'static> Actionable for Split<P, Q, R> {
    type NextAction = Self;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>, R: Scoped<N>> Scoped<N> for Split<P, Q, R> {}

impl<N: Unary, P: Subst<S, N>, Q: Subst<S, N>, R: Subst<S, N>, S> Subst<S, N> for Split<P, Q, R> {
    type Substituted = Split<P::Substituted, Q::Substituted, R::Substituted>;
}

impl<N: Unary, P: 'static, Q: 'static, R: Then<S, N>, S> Then<S, N> for Split<P, Q, R> {
    type Combined = Split<P, Q, R::Combined>;
}

impl<N: Unary, Level: Unary, P: Lift<N, Level>, Q: Lift<N, Level>, R: Lift<N, Level>> Lift<N, Level>
    for Split<P, Q, R>
{
    type Lifted = Split<P::Lifted, Q::Lifted, R::Lifted>;
}
