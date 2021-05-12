use std::{any::Any, marker::PhantomData};

use super::sealed::IsSession;
use super::*;

/// Call the session `P` as a subroutine using [`call`](crate::Chan::call), then do the session `Q`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Call<P, Q>(PhantomData<fn() -> P>, PhantomData<fn() -> Q>);

impl<P, Q> Default for Call<P, Q> {
    fn default() -> Self {
        Call(PhantomData, PhantomData)
    }
}

impl<P: Any, Q: Any> IsSession for Call<P, Q> {}

impl<P: HasDual, Q: HasDual> HasDual for Call<P, Q> {
    type DualSession = Call<P::DualSession, Q::DualSession>;
}

impl<P: Default + 'static, Q: Default + 'static> Actionable for Call<P, Q> {
    type NextAction = Self;
}

impl<N: Unary, P: Scoped<N>, Q: Scoped<N>> Scoped<N> for Call<P, Q> {}

impl<N: Unary, P: Subst<R, N>, Q: Subst<R, N>, R> Subst<R, N> for Call<P, Q> {
    type Substituted = Call<P::Substituted, Q::Substituted>;
}

impl<N: Unary, P: Default + 'static, Q: Then<R, N>, R> Then<R, N> for Call<P, Q> {
    type Combined = Call<P, Q::Combined>;
}

impl<N: Unary, Level: Unary, P: Lift<N, Level>, Q: Lift<N, Level>> Lift<N, Level> for Call<P, Q> {
    type Lifted = Call<P::Lifted, Q::Lifted>;
}
