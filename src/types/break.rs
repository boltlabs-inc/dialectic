use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Break out of a [`Loop`]. The type-level index points to the loop to be broken, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Break<N: Unary = Z>(pub N);

/// [`Break`] is a constructor for a session type.
impl<N: Unary + Any> IsSession for Break<N> {}

/// All [`Break`]s are valid session types.
impl<N: Unary + Any> Session for Break<N> {
    /// [`Break`] is self-dual.
    type Dual = Break<N>;
}

/// A [`Break`] is well-[`Scoped`] if it refers to a loop that it is inside of.
impl<N: Unary + Any, M: Unary + Any> Scoped<M> for Break<N> where N: LessThan<M> {}

/// When in the outermost loop, [`Break`] exits that loop and finishes the session.
impl<P> Actionable<(P, ())> for Break<Z>
where
    P: Scoped<S<Z>>,
    (P, ()): Environment + 'static,
    Z: LessThan<<(P, ()) as Environment>::Depth>,
{
    type Action = Done;
    type Env = ();
}

/// Break from a non-outermost loop: continue whatever loop we broke into.
impl<P, Q, Rest> Actionable<(P, (Q, Rest))> for Break<Z>
where
    Q: Actionable<(Q, Rest)>,
    P: Scoped<S<S<<Rest as Environment>::Depth>>>,
    Q: Scoped<S<<Rest as Environment>::Depth>>,
    Q::Env: Environment,
    (P, (Q, Rest)): Environment + 'static,
    (Q, Rest): Environment,
    Rest: Environment,
    Z: LessThan<<(P, (Q, Rest)) as Environment>::Depth>,
{
    type Action = Q::Action;
    type Env = Q::Env;
}

/// Inductive case: break one less level from one less loop.
impl<N: Unary, P, Rest> Actionable<(P, Rest)> for Break<S<N>>
where
    P: Scoped<S<<Rest as Environment>::Depth>>,
    N: LessThan<Rest::Depth>,
    Break<N>: Actionable<Rest>,
    (P, Rest): Environment + 'static,
    Rest: Environment,
    S<N>: LessThan<<(P, Rest) as Environment>::Depth>,
{
    type Action = <Break<N> as Actionable<Rest>>::Action;
    type Env = <Break<N> as Actionable<Rest>>::Env;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_all_closed_sessions;

    #[test]
    fn break_good() {
        assert_all_closed_sessions!(
            Loop<Break>,
            Loop<Loop<Break<_1>>>,
            Loop<Send<String, Break>>,
        );
    }
}
