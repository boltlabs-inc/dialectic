use super::sealed::IsSession;
use super::*;
use crate::unary::{self, Compare, Number, ToConstant, ToUnary};

/// Helper trait for converting an unary number type corresponding to some const generic `usize` `N`
/// into a corresponding `Continue<N>`. It is not possible to do this any other way at the moment
/// because const generic parameters cannot be associated constants and without associated constants
/// as such we are forced to use this method to produce a type uniquely dependent on one without
/// having the constant itself available.
pub trait ToContinue {
    /// The resulting `Continue<N>` type.
    type AsContinue: IsSession;
}

impl<const I: usize> ToContinue for Number<I> {
    type AsContinue = Continue<I>;
}

impl ToContinue for Z {
    type AsContinue = <<Z as ToConstant>::AsConstant as ToContinue>::AsContinue;
}

impl<N: Unary, Const> ToContinue for S<N>
where
    Self: ToConstant<AsConstant = Const>,
    Const: ToContinue,
{
    type AsContinue = Const::AsContinue;
}

/// Repeat a [`Loop`]. The type-level index points to the loop to be repeated, counted from the
/// innermost starting at `0`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Continue<const I: usize>;

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

impl<N: Unary, M: Unary, P: Unary, Summed, Level: Unary, const I: usize> Lift<N, Level>
    for Continue<I>
where
    Number<I>: ToUnary<AsUnary = M>,
    (M, N): unary::Add<Result = P>,
    <(M, N) as unary::Add>::Result: ToContinue<AsContinue = Summed>,
    (M, Level): Compare<Continue<I>, Summed, Summed>,
    <(M, Level) as Compare<Continue<I>, Summed, Summed>>::Result: 'static,
{
    type Lifted = <(M, Level) as Compare<Continue<I>, Summed, Summed>>::Result;
}
