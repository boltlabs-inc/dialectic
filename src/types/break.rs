use super::sealed::IsSession;
use super::*;

/// Break out of a [`Loop`]. The type-level index points to the loop to be broken, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Break<N: Unary = Z>(pub N);

/// [`Break`] is a constructor for a session type.
impl<N: Unary> IsSession for Break<N> {}

/// All [`Break`]s are valid session types.
impl<N: Unary> Session for Break<N> {
    /// [`Break`] is self-dual.
    type Dual = Break<N>;
}

/// A [`Break`] is well-[`Scoped`] if it refers to a loop that it is inside of.
impl<N: Unary, M: Unary> Scoped<M> for Break<N> where N: LessThan<M> {}

/// When in the outermost loop, [`Break`] exits that loop and finishes the session.
impl<P> Actionable<(P, ())> for Break<Z>
where
    (P, ()): Environment,
    <(P, ()) as EachSession>::Dual: Environment,
    Z: LessThan<<(P, ()) as Environment>::Depth>, // this is always true but Rust doesn't know it
{
    type Action = Done;
    type Env = ();
}

/// Break from a non-outermost loop: continue whatever loop we broke into.
impl<P, Q, Rest> Actionable<(P, (Q, Rest))> for Break<Z>
where
    Q: Actionable<(Q, Rest)>,
    Q: Scoped<S<<Rest as Environment>::Depth>> + Scoped<S<<Rest::Dual as Environment>::Depth>>,
    P: Scoped<S<S<<Rest as Environment>::Depth>>>,
    P: Scoped<S<S<<Rest::Dual as Environment>::Depth>>>,
    P::Dual: Scoped<S<S<<Rest as Environment>::Depth>>>,
    P::Dual: Scoped<S<S<<Rest::Dual as Environment>::Depth>>>,
    Q::Dual: Scoped<S<<Rest as Environment>::Depth>>,
    Q::Dual: Scoped<S<<Rest::Dual as Environment>::Depth>>,
    Q::Env: Environment,
    Rest: Environment,
    Rest::Dual: Environment,
    <Q::Env as EachSession>::Dual: Environment,
{
    type Action = Q::Action;
    type Env = Q::Env;
}

/// Inductive case: break one less level from one less loop.
impl<N: Unary, P, Rest> Actionable<(P, Rest)> for Break<S<N>>
where
    N: LessThan<Rest::Depth>,
    Break<N>: Actionable<Rest>,
    P: Scoped<S<Rest::Depth>> + Scoped<S<<Rest::Dual as Environment>::Depth>>,
    P::Dual: Scoped<S<Rest::Depth>> + Scoped<S<<Rest::Dual as Environment>::Depth>>,
    Rest: Environment,
    Rest::Dual: Environment,
    <<Break<N> as Actionable<Rest>>::Env as EachSession>::Dual: Environment,
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
