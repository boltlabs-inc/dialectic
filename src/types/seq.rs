use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Sequence two sessions `P` and `Q` together using [`seq`](Chan::seq).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Seq<P, Q>(pub P, pub Q);

impl<P: Any, Q: Any> IsSession for Seq<P, Q> {}

impl<P: HasDual, Q: HasDual> HasDual for Seq<P, Q> {
    type DualSession = Seq<P::DualSession, Q::DualSession>;
}

impl<P: 'static, Q: 'static> Actionable for Seq<P, Q> {
    type NextAction = Self;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Seq<P, Q> {}

impl<N: Unary, P: Subst<R, N>, Q: Subst<R, N>, R> Subst<R, N> for Seq<P, Q> {
    type Substituted = Seq<P::Substituted, Q::Substituted>;
}
