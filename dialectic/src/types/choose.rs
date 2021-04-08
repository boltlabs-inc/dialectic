use std::{any::Any, marker::PhantomData};

use super::sealed::IsSession;
use super::*;

use crate::tuple::{List, Tuple};

/// Actively [`choose`](crate::Chan::choose) between any of the protocols in the tuple
/// `Choices`.
///
/// At most 128 choices can be presented to a `Choose` type; to choose from more options, nest
/// `Choose`s within each other.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Choose<Carrier, Choices>(PhantomData<fn() -> (Choices, Carrier)>);

impl<Carrier, Choices> Default for Choose<Carrier, Choices> {
    fn default() -> Self {
        Choose(PhantomData)
    }
}

impl<Choices: Any, Carrier: 'static> IsSession for Choose<Carrier, Choices> {}

impl<Choices, Carrier> HasDual for Choose<Carrier, Choices>
where
    Choices: Any + Tuple,
    Choices::AsList: EachHasDual,
    <Choices::AsList as EachHasDual>::Duals: List + EachHasDual,
    Carrier: 'static,
{
    type DualSession = Offer<Carrier, <<Choices::AsList as EachHasDual>::Duals as List>::AsTuple>;
}

impl<Choices: 'static, Carrier: 'static> Actionable for Choose<Carrier, Choices> {
    type NextAction = Self;
}

impl<N: Unary, Choices: Tuple + 'static, Carrier: 'static> Scoped<N> for Choose<Carrier, Choices> where
    Choices::AsList: EachScoped<N>
{
}

impl<N: Unary, P, Choices, Carrier> Subst<P, N> for Choose<Carrier, Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachSubst<P, N>,
    <Choices::AsList as EachSubst<P, N>>::Substituted: List,
    Carrier: 'static,
{
    type Substituted =
        Choose<<<Choices::AsList as EachSubst<P, N>>::Substituted as List>::AsTuple, Carrier>;
}

impl<N: Unary, P, Choices, Carrier> Then<P, N> for Choose<Carrier, Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachThen<P, N>,
    <Choices::AsList as EachThen<P, N>>::Combined: List,
    Carrier: 'static,
{
    type Combined =
        Choose<<<Choices::AsList as EachThen<P, N>>::Combined as List>::AsTuple, Carrier>;
}

impl<N: Unary, Level: Unary, Choices, Carrier> Lift<N, Level> for Choose<Carrier, Choices>
where
    Choices: Tuple + 'static,
    Choices::AsList: EachLift<N, Level>,
    <Choices::AsList as EachLift<N, Level>>::Lifted: List,
    Carrier: 'static,
{
    type Lifted =
        Choose<<<Choices::AsList as EachLift<N, Level>>::Lifted as List>::AsTuple, Carrier>;
}
