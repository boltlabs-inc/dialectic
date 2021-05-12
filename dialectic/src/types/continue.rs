use super::sealed::IsSession;
use super::*;
use crate::unary::{self, Compare, Number, ToConstant, ToUnary};

/// Repeat a [`Loop`]. The type-level index points to the loop to be repeated, counted from the
/// innermost starting at `0`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Continue<const I: usize>(());

impl<const I: usize> IsSession for Continue<I> {}

impl<const I: usize> HasDual for Continue<I> {
    type DualSession = Continue<I>;
}

impl<N: Unary, M: Unary, const I: usize> Scoped<N> for Continue<I>
where
    Number<I>: ToUnary<AsUnary = M>,
    M: LessThan<N>,
{
}

impl<P, N: Unary, M: Unary, const I: usize> Subst<P, N> for Continue<I>
where
    Number<I>: ToUnary<AsUnary = M>,
    (N, M): Compare<Continue<I>, P, Continue<I>>,
    <(N, M) as Compare<Continue<I>, P, Continue<I>>>::Result: 'static,
{
    type Substituted = <(N, M) as Compare<Continue<I>, P, Continue<I>>>::Result;
}

impl<P, N: Unary, const I: usize> Then<P, N> for Continue<I> {
    type Combined = Continue<I>;
}

impl<N: Unary, M: Unary, P: Unary, Level: Unary, const I: usize, const J: usize> Lift<N, Level>
    for Continue<I>
where
    Number<I>: ToUnary<AsUnary = M>,
    (M, N): unary::Add<Result = P>,
    P: ToConstant<AsConstant = Number<J>>,
    (M, Level): Compare<Continue<I>, Continue<J>, Continue<J>>,
{
    type Lifted = <(M, Level) as Compare<Continue<I>, Continue<J>, Continue<J>>>::Result;
}
