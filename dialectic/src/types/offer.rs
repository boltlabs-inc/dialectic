use std::{any::Any, marker::PhantomData};

use super::sealed::IsSession;
use super::*;

use crate::tuple::{List, Tuple};

/// Passively [`offer!`](crate::offer) a choice between any of the protocols in the tuple `Choices`.
///
/// At most 128 choices can be offered in a single `Offer` type; to supply more options, nest
/// `Offer`s within each other.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Offer<Choices>(PhantomData<fn() -> Choices>);

impl<Choices> Default for Offer<Choices> {
    fn default() -> Self {
        Offer(PhantomData)
    }
}

impl<Choices: Any> IsSession for Offer<Choices> {}

impl<Choices: Any> HasDual for Offer<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachHasDual,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
{
    type DualSession = Choose<<<Choices::AsList as EachHasDual>::Duals as List>::AsTuple>;
}

impl<Choices: 'static> Actionable for Offer<Choices> {
    type NextAction = Self;
}

impl<N: Unary, Choices: Tuple + 'static> Scoped<N> for Offer<Choices> where
    Choices::AsList: EachScoped<N>
{
}

impl<N: Unary, P, Choices> Subst<P, N> for Offer<Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachSubst<P, N>,
    <Choices::AsList as EachSubst<P, N>>::Substituted: List,
{
    type Substituted = Offer<<<Choices::AsList as EachSubst<P, N>>::Substituted as List>::AsTuple>;
}

impl<N: Unary, P, Choices> Then<P, N> for Offer<Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachThen<P, N>,
    <Choices::AsList as EachThen<P, N>>::Combined: List,
{
    type Combined = Offer<<<Choices::AsList as EachThen<P, N>>::Combined as List>::AsTuple>;
}

impl<N: Unary, Level: Unary, Choices> Lift<N, Level> for Offer<Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachLift<N, Level>,
    <Choices::AsList as EachLift<N, Level>>::Lifted: List,
{
    type Lifted = Offer<<<Choices::AsList as EachLift<N, Level>>::Lifted as List>::AsTuple>;
}
