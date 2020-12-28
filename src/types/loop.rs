use super::sealed::IsSession;
use super::*;

/// Label a loop point, which can be reiterated with [`Continue`], or implicitly with [`Done`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Loop<P>(pub P);

impl<P> IsSession for Loop<P> {}

impl<P: Session> Session for Loop<P> {
    type Dual = Loop<P::Dual>;
}

impl<N: Unary, P: Scoped<S<N>>> Scoped<N> for Loop<P> {}

impl<P, E> Actionable<E> for Loop<P>
where
    E: Environment,
    E::Dual: Environment,
    P: Actionable<(P, E)>,
    P: Scoped<S<E::Depth>>,
    P: Scoped<S<<E::Dual as Environment>::Depth>>,
    P::Dual: Scoped<S<E::Depth>>,
    P::Dual: Scoped<S<<E::Dual as Environment>::Depth>>,
    <P::Env as EachSession>::Dual: Environment,
{
    type Action = <P as Actionable<(P, E)>>::Action;
    type Env = <P as Actionable<(P, E)>>::Env;
}