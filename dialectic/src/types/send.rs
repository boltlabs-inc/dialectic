use super::sealed::IsSession;
use crate::types::*;
use std::{any::Any, marker::PhantomData};

/// Send a message of type `T` using [`send`](crate::Chan::send), then continue with
/// protocol `P`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Send<T, P>(PhantomData<fn() -> T>, PhantomData<fn() -> P>);

impl<T, P> Default for Send<T, P> {
    fn default() -> Self {
        Send(PhantomData, PhantomData)
    }
}

impl<T: Any, P: Any> IsSession for Send<T, P> {}

impl<T: Any, P: HasDual> HasDual for Send<T, P> {
    type DualSession = Recv<T, P::DualSession>;
}

impl<T: 'static, P: 'static> Actionable for Send<T, P> {
    type NextAction = Self;
}

impl<T: 'static, N: Unary, P: Scoped<N>> Scoped<N> for Send<T, P> {}

impl<N: Unary, T: 'static, P: Subst<Q, N>, Q> Subst<Q, N> for Send<T, P> {
    type Substituted = Send<T, P::Substituted>;
}

impl<N: Unary, T: 'static, P: Then<Q, N>, Q> Then<Q, N> for Send<T, P> {
    type Combined = Send<T, P::Combined>;
}

impl<N: Unary, T: 'static, P: Lift<N, Level>, Level: Unary> Lift<N, Level> for Send<T, P> {
    type Lifted = Send<T, P::Lifted>;
}
