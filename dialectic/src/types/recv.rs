use super::sealed::IsSession;
use crate::types::*;
use std::{any::Any, marker::PhantomData};

/// Receive a message of type `T` using [`recv`](crate::Chan::recv), then continue with
/// protocol `P`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Recv<T, P>(PhantomData<fn() -> T>, PhantomData<fn() -> P>);

impl<T, P> Default for Recv<T, P> {
    fn default() -> Self {
        Recv(PhantomData, PhantomData)
    }
}

impl<T: Any, P: Any> IsSession for Recv<T, P> {}

impl<T: Any, P: HasDual> HasDual for Recv<T, P> {
    type DualSession = Send<T, P::DualSession>;
}

impl<T: 'static, P: 'static> Actionable for Recv<T, P> {
    type NextAction = Self;
}

impl<T: 'static, N: Unary, P: Scoped<N>> Scoped<N> for Recv<T, P> {}

impl<N: Unary, T: 'static, P: Subst<Q, N>, Q> Subst<Q, N> for Recv<T, P> {
    type Substituted = Recv<T, P::Substituted>;
}

impl<N: Unary, T: 'static, P: Then<Q, N>, Q> Then<Q, N> for Recv<T, P> {
    type Combined = Recv<T, P::Combined>;
}

impl<N: Unary, T: 'static, P: Lift<N, Level>, Level: Unary> Lift<N, Level> for Recv<T, P> {
    type Lifted = Recv<T, P::Lifted>;
}
