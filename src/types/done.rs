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

impl Actionable<()> for Done {
    type Action = Done;
    type Env = ();
}

impl<P, Rest> Actionable<((Done, P), Rest)> for Done
where
    P: Scoped<S<<Rest as Environment>::Depth>>,
    Rest: Environment,
{
    type Action = Done;
    type Env = ();
}

/// When inside a `Loop`, `Done` repeats the innermost loop.
impl<P, Rest> Actionable<((Continue, P), Rest)> for Done
where
    P: Scoped<S<<Rest as Environment>::Depth>>,
    Continue: Actionable<((Continue, P), Rest)>,
    Rest: Environment,
{
    type Action = <Continue as Actionable<((Continue, P), Rest)>>::Action;
    type Env = <Continue as Actionable<((Continue, P), Rest)>>::Env;
}
