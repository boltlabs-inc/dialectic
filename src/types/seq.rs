use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Sequence two sessions `P` and `Q` together using [`seq`](Chan::seq).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Seq<P, Q = Done>(pub P, pub Q);

impl<P: Any, Q: Any> IsSession for Seq<P, Q> {}

impl<P: HasDual, Q: HasDual> HasDual for Seq<P, Q> {
    type DualSession = Seq<P::DualSession, Q::DualSession>;
}

impl<P, Q> Actionable for Seq<P, Q> {
    type NextAction = Self;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Seq<P, Q> {}

/// Importantly, we require that `P: Subst<R, N, Done>`, which means [`Done`] doesn't implicitly
/// loop at the outermost level of the first part of a [`Seq`].
impl<N: Unary, Mode, P: Subst<R, N, Done>, Q: Subst<R, N, Mode>, R> Subst<R, N, Mode>
    for Seq<P, Q>
{
    type Substituted = Seq<P::Substituted, Q::Substituted>;
}
