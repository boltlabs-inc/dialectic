use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Actively [`choose`](crate::CanonicalChan::choose) between any of the protocols in the tuple
/// `Choices`.
///
/// At most 128 choices can be presented to a `Choose` type; to choose from more options, nest
/// `Choose`s within each other.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Choose<Choices>(pub Choices);

impl<Choices: Any> IsSession for Choose<Choices> {}

impl<Choices: Any> HasDual for Choose<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachHasDual,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
{
    type Dual = Offer<<<Choices::AsList as EachHasDual>::Duals as List>::AsTuple>;
}

impl<Choices> Actionable for Choose<Choices> {
    type Action = Self;
}

impl<N: Unary, Choices: Tuple> Scoped<N> for Choose<Choices> where Choices::AsList: EachScoped<N> {}

impl<N: Unary, Mode, P, Choices> Subst<P, N, Mode> for Choose<Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachHasDual + EachSubst<P, N, Mode>,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
    <Choices::AsList as EachSubst<P, N, Mode>>::Substituted: List + EachHasDual,
    <<Choices::AsList as EachSubst<P, N, Mode>>::Substituted as EachHasDual>::Duals: List,
{
    type Substituted =
        Choose<<<Choices::AsList as EachSubst<P, N, Mode>>::Substituted as List>::AsTuple>;
}
