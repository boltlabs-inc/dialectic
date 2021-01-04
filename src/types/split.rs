use super::sealed::IsSession;
use super::*;

/// Split the connection into send-only and receive-only halves using
/// [`split`](crate::CanonicalChan::split). These can subsequently be rejoined using
/// [`unsplit_with`](crate::CanonicalChan::unsplit_with).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Split<P, Q>(pub P, pub Q);

impl<P, Q> IsSession for Split<P, Q> {}

impl<P: Session, Q: Session> Session for Split<P, Q> {
    /// Note how the dual flips the position of `P` and `Q`, because `P::Dual` is a receiving
    /// session, and therefore belongs on the right of the split, and `Q::Dual` is a sending
    /// session, and therefore belongs on the left of the split.
    type Dual = Split<Q::Dual, P::Dual>;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Split<P, Q> {}

impl<E, P, Q> Actionable<E> for Split<P, Q>
where
    P: Scoped<E::Depth>,
    Q: Scoped<E::Depth>,
    E: Environment,
{
    type Action = Split<P, Q>;
    type Env = E;
}
