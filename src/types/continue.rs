use super::sealed::IsSession;
use super::*;

/// Repeat a [`Loop`]. The type-level index points to the loop to be repeated, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Continue<N: Unary = Z>(pub N);

impl<N: Unary> IsSession for Continue<N> {}

impl<N: Unary> Session for Continue<N> {
    type Dual = Continue<N>;
}

impl<N: Unary, M: Unary> Scoped<M> for Continue<N> where N: LessThan<M> {}

impl<E, N: Unary> Actionable<E> for Continue<N>
where
    E: Select<N> + Environment,
    Continue<N>: Scoped<E::Depth>,
    E::Selected: Actionable<(E::Selected, E::Remainder)>,
    E::Selected: Scoped<S<<E::Remainder as Environment>::Depth>>,
    <E::Selected as Session>::Dual: Scoped<S<<E::Remainder as Environment>::Depth>>,
    E::Selected: Scoped<S<<<E::Remainder as EachSession>::Dual as Environment>::Depth>>,
    <E::Selected as Session>::Dual:
        Scoped<S<<<E::Remainder as EachSession>::Dual as Environment>::Depth>>,
    E::Dual: Environment,
    E::Remainder: Environment,
    <E::Remainder as EachSession>::Dual: Environment,
    <<E::Selected as Actionable<(E::Selected, E::Remainder)>>::Env as EachSession>::Dual:
        Environment,
{
    type Action = <E::Selected as Actionable<(E::Selected, E::Remainder)>>::Action;
    type Env = <E::Selected as Actionable<(E::Selected, E::Remainder)>>::Env;
}
