use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Actively [`choose`](crate::Chan::choose) between any of the protocols in the tuple
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
    type DualSession = Offer<<<Choices::AsList as EachHasDual>::Duals as List>::AsTuple>;
}

impl<Choices: 'static> Actionable for Choose<Choices> {
    type NextAction = Self;
}

impl<N: Unary, Choices: Tuple + 'static> Scoped<N> for Choose<Choices> where
    Choices::AsList: EachScoped<N>
{
}

impl<N: Unary, P, Choices> Subst<P, N> for Choose<Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachHasDual + EachSubst<P, N>,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
    <Choices::AsList as EachSubst<P, N>>::Substituted: List + EachHasDual,
    <<Choices::AsList as EachSubst<P, N>>::Substituted as EachHasDual>::Duals: List,
{
    type Substituted = Choose<<<Choices::AsList as EachSubst<P, N>>::Substituted as List>::AsTuple>;
}
