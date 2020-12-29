use super::sealed::IsSession;
use super::*;

/// Passively [`offer!`](crate::offer) a choice between any of the protocols in the tuple `Choices`.
///
/// At most 128 choices can be offered in a single `Offer` type; to supply more options, nest
/// `Offer`s within each other.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Offer<Choices>(pub Choices);

impl<Choices> IsSession for Offer<Choices> {}

impl<Choices> Session for Offer<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
{
    type Dual = Choose<<<Choices::AsList as EachSession>::Dual as List>::AsTuple>;
}

impl<N: Unary, Choices: Tuple> Scoped<N> for Offer<Choices>
where
    Choices::AsList: EachScoped<N>,
    <Choices::AsList as EachSession>::Dual: List,
{
}

impl<E, Choices> Actionable<E> for Offer<Choices>
where
    Choices: Tuple,
    Choices::AsList: EachSession,
    <Choices::AsList as EachSession>::Dual: List + EachSession,
    Choices::AsList: EachScoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Offer<Choices>;
    type Env = E;
}
