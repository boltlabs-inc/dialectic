use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Split the connection into send-only and receive-only halves using
/// [`split`](crate::Chan::split).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Split<P, Q>(pub P, pub Q);

impl<P: Any, Q: Any> IsSession for Split<P, Q> {}

impl<P: HasDual, Q: HasDual> HasDual for Split<P, Q> {
    /// Note how the dual flips the position of `P` and `Q`, because `P::Dual` is a receiving
    /// session, and therefore belongs on the right of the split, and `Q::Dual` is a sending
    /// session, and therefore belongs on the left of the split.
    type DualSession = Split<Q::DualSession, P::DualSession>;
}

impl<P, Q> Actionable for Split<P, Q> {
    type NextAction = Self;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Split<P, Q> {}

impl<N: Unary, Mode, P: Subst<R, N, Mode>, Q: Subst<R, N, Mode>, R> Subst<R, N, Mode>
    for Split<P, Q>
{
    type Substituted = Split<P::Substituted, Q::Substituted>;
}
