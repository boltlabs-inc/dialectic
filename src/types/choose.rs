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

impl<Choices: Any> Session for Choose<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
{
    type Dual = Offer<<<Choices::AsList as EachSession>::Dual as List>::AsTuple>;
}

impl<N: Unary, Choices: Tuple + Any> Scoped<N> for Choose<Choices>
where
    Choices::AsList: EachScoped<N>,
    <Choices::AsList as EachSession>::Dual: List,
{
}

impl<E, Choices: Any> Actionable<E> for Choose<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
    Choices::AsList: EachScoped<E::Depth>,
    E: Environment,
{
    type Action = Choose<Choices>;
    type Env = E;
}
