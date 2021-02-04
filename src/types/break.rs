use std::any::Any;

use super::sealed::IsSession;
use super::*;
use crate::unary::Compare;

/// Break out of a [`Loop`]. The type-level index points to the loop to be broken, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Break<N: Unary = Z>(pub N);

/// [`Break`] is a constructor for a session type.
impl<N: Unary + Any> IsSession for Break<N> {}

impl<N: Unary + Any> HasDual for Break<N> {
    type DualSession = Break<N>;
}

impl<N: Unary, M: Unary> Scoped<N> for Break<M> where M: LessThan<N> {}

// The `Subst` impl for `Break` is kind of tricky. Every time we go through a `Loop`, we need to
// transform all the `Break`s at the outermost level into `Done`s, but we need to make sure we're
// well-`Scoped` at all times. This means that every `Subst` decrements the de Bruijn index of the
// `Break` it hits, unless that `Break` matches the thing being substituted, in which case
// substitution occurs. Therefore, in any well-`Scoped` session type, the outermost `Break`s will
// always end up `Done` by the time we reach them.

impl<P> Subst<P, Z> for Break<Z> {
    type Substituted = Done;
}

impl<P, N: Unary> Subst<P, S<N>> for Break<Z> {
    type Substituted = Break<Z>;
}

impl<P, N: Unary, M: Unary> Subst<P, S<N>> for Break<S<M>>
where
    (M, S<N>): Compare<Break<M>, P, Break<S<M>>>,
    <(M, S<N>) as Compare<Break<M>, P, Break<S<M>>>>::Result: 'static,
{
    type Substituted = <(M, S<N>) as Compare<Break<M>, P, Break<S<M>>>>::Result;
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

        static_assertions::assert_impl_all!(Loop<Loop<Send<String, Break<_0>>>>: Session<Action = Send<String, Loop<Loop<Send<String, Break<_0>>>>>>);
    }
}
