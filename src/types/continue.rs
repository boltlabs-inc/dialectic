use std::any::Any;

use super::sealed::IsSession;
use super::*;

/// Repeat a [`Loop`]. The type-level index points to the loop to be repeated, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Continue<N: Unary = Z>(pub N);

impl<N: Unary + Any> IsSession for Continue<N> {}

impl<N: Unary + Any> Session for Continue<N> {
    type Dual = Continue<N>;
}

impl<N: Unary + Any, M: Unary + Any> Scoped<M> for Continue<N> where N: LessThan<M> {}

impl<E, P, Rest, N: Unary> Actionable<E> for Continue<N>
where
    E: Select<N, Selected = P, Remainder = Rest> + Environment,
    Continue<N>: Scoped<E::Depth>,
    P: Actionable<(P, Rest)>,
    P: Scoped<S<<Rest as Environment>::Depth>>,
    P::Dual: Scoped<S<Rest::Depth>>,
    (P, Rest): Environment,
    Rest: Environment,
{
    type Action = <P as Actionable<(P, Rest)>>::Action;
    type Env = <P as Actionable<(P, Rest)>>::Env;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_all_closed_sessions;

    #[test]
    fn break_good() {
        assert_all_closed_sessions!(
            Loop<Send<i64, Continue>>,
            Loop<Send<i64, Loop<Continue<_1>>>>,
            Loop<Send<String, Continue>>,
        );
    }
}
