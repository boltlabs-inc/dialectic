use super::sealed::IsSession;
use super::*;

/// Complete a session. The only thing to do with a [`Chan`] at its `Done` is to drop it.
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
