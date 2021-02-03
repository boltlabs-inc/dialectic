use std::any::Any;

use super::sealed::{IsSession, SubstMode};
use super::*;
use crate::unary::Compare;

/// Repeat a [`Loop`]. The type-level index points to the loop to be repeated, counted from the
/// innermost starting at [`Z`].
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Continue<N: Unary = Z>(pub N);

impl<N: Unary + Any> IsSession for Continue<N> {}

impl<N: Unary + Any> HasDual for Continue<N> {
    type DualSession = Continue<N>;
}

impl<N: Unary, M: Unary> Scoped<N> for Continue<M> where M: LessThan<N> {}

impl<P, Mode, N: Unary, M: Unary> Subst<P, N, Mode> for Continue<M>
where
    (N, M): Compare<Continue<M>, P, Continue<M>>,
    <(N, M) as Compare<Continue<M>, P, Continue<M>>>::Result: 'static,
{
    type Substituted = <(N, M) as Compare<Continue<M>, P, Continue<M>>>::Result;
}

impl SubstMode for Continue {}

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
