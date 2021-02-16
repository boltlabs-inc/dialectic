use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Call the session `P` as a subroutine using [`call`](Chan::call), then do the session `Q`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Call<P, Q>(pub P, pub Q);

impl<P: Any, Q: Any> IsSession for Call<P, Q> {}

impl<P: HasDual, Q: HasDual> HasDual for Call<P, Q> {
    type DualSession = Call<P::DualSession, Q::DualSession>;
}

impl<P: 'static, Q: 'static> Actionable for Call<P, Q> {
    type NextAction = Self;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Call<P, Q> {}

impl<N: Unary, P: Subst<R, N>, Q: Subst<R, N>, R> Subst<R, N> for Call<P, Q> {
    type Substituted = Call<P::Substituted, Q::Substituted>;
}
