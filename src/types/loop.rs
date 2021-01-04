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
    P: Actionable<((Continue, P), E)>,
    P: Scoped<S<E::Depth>>,
{
    type Action = <P as Actionable<((Continue, P), E)>>::Action;
    type Env = <P as Actionable<((Continue, P), E)>>::Env;
}
