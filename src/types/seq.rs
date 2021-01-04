use super::sealed::IsSession;
use super::*;

/// Sequence two sessions together, analogous to `;` in Rust.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Seq<P, Q = Done>(pub P, pub Q);

impl<P, Q> IsSession for Seq<P, Q> {}

impl<P: Session, Q: Session> Session for Seq<P, Q> {
    type Dual = Seq<P::Dual, Q::Dual>;
}

impl<P: Scoped<N>, Q: Scoped<N>, N: Unary> Scoped<N> for Seq<P, Q> {}

impl<E, P, Q> Actionable<E> for Seq<P, Q>
where
    P: Scoped<E::Depth>,
    Q: Scoped<E::Depth>,
    E: Environment,
{
    type Action = Seq<P, Q>;
    type Env = E;
}
