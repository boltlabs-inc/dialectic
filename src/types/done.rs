use super::sealed::IsSession;
use super::*;

/// A finished session. The only thing to do with a [`Chan`] when it is `Done` is to drop it or,
/// preferably, [`close`](CanonicalChan::close) it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Done;

impl IsSession for Done {}

impl Session for Done {
    type Dual = Done;
}

impl<N: Unary> Scoped<N> for Done {}

impl<E: Environment> Actionable<E> for Done {
    type Action = Done;
    type Env = ();
}
