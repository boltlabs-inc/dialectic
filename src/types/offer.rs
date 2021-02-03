use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Passively [`offer!`](crate::offer) a choice between any of the protocols in the tuple `Choices`.
///
/// At most 128 choices can be offered in a single `Offer` type; to supply more options, nest
/// `Offer`s within each other.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Offer<Choices>(pub Choices);

impl<Choices: Any> IsSession for Offer<Choices> {}

impl<Choices: Any> HasDual for Offer<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachHasDual,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
{
    type DualSession = Choose<<<Choices::AsList as EachHasDual>::Duals as List>::AsTuple>;
}

impl<Choices> Actionable for Offer<Choices> {
    type NextAction = Self;
}

impl<N: Unary, Choices: Tuple> Scoped<N> for Offer<Choices> where Choices::AsList: EachScoped<N> {}

impl<N: Unary, Mode: sealed::SubstMode, P, Choices> Subst<P, N, Mode> for Offer<Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachHasDual + EachSubst<P, N, Mode>,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
    <Choices::AsList as EachSubst<P, N, Mode>>::Substituted: List + EachHasDual,
    <<Choices::AsList as EachSubst<P, N, Mode>>::Substituted as EachHasDual>::Duals: List,
{
    type Substituted =
        Offer<<<Choices::AsList as EachSubst<P, N, Mode>>::Substituted as List>::AsTuple>;
}
