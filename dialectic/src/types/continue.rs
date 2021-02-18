use std::any::Any;

use super::sealed::IsSession;
use super::*;
use crate::unary::{self, Compare};

/// Repeat a [`Loop`]. The type-level index points to the loop to be repeated, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Continue<N: Unary = Z>(pub N);

impl<N: Unary + Any> IsSession for Continue<N> {}

impl<N: Unary + Any> HasDual for Continue<N> {
    type DualSession = Continue<N>;
}

impl<N: Unary, M: Unary> Scoped<N> for Continue<M> where M: LessThan<N> {}

impl<P, N: Unary, M: Unary> Subst<P, N> for Continue<M>
where
    (N, M): Compare<Continue<M>, P, Continue<M>>,
    <(N, M) as Compare<Continue<M>, P, Continue<M>>>::Result: 'static,
{
    type Substituted = <(N, M) as Compare<Continue<M>, P, Continue<M>>>::Result;
}

impl<P, M: Unary, N: Unary> Then<P, N> for Continue<M> {
    type Combined = Continue<M>;
}

impl<N: Unary, M: Unary, Level: Unary> Lift<N, Level> for Continue<M>
where
    (M, Level): Compare<
        Continue<M>,
        Continue<<(M, N) as unary::Add>::Result>,
        Continue<<(M, N) as unary::Add>::Result>,
    >,
    (M, N): unary::Add,
{
    type Lifted = <(M, Level) as Compare<
        Continue<M>,
        Continue<<(M, N) as unary::Add>::Result>,
        Continue<<(M, N) as unary::Add>::Result>,
    >>::Result;
}
