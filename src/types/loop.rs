use super::sealed::IsSession;
use super::*;

/// Label a loop point, which can be reiterated with [`Continue`], or broken out of with [`Break`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Loop<P>(pub P);

impl<P: IsSession> IsSession for Loop<P> {}

impl<P> HasDual for Loop<P>
where
    P: HasDual,
{
    type DualSession = Loop<<P as HasDual>::DualSession>;
}

impl<N: Unary, P: Scoped<S<N>>> Scoped<N> for Loop<P> {}

impl<P> Actionable for Loop<P>
where
    P: Subst<Loop<P>, Z>,
    P::Substituted: Actionable,
{
    type NextAction = <P::Substituted as Actionable>::NextAction;
}

impl<P, Q, N: Unary> Subst<Q, N> for Loop<P>
where
    P: Subst<Q, S<N>>,
    <P as Subst<Q, S<N>>>::Substituted: HasDual,
{
    type Substituted = Loop<<P as Subst<Q, S<N>>>::Substituted>;
}
