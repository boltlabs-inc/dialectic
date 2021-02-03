use super::sealed::IsSession;
use crate::types::*;
use std::{any::Any, marker::PhantomData};

/// Send a message of type `T` using [`send`](crate::Chan::send), then continue with
/// protocol `P`.
///
/// # Notes
///
/// A session ending with a `Send` can be abbreviated: `Send<String>` is shorthand for `Send<String,
/// Done>`.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Send<T, P = Done>(pub PhantomData<T>, pub P);

impl<T: Any, P: Any> IsSession for Send<T, P> {}

impl<T: Any, P: HasDual> HasDual for Send<T, P> {
    type DualSession = Recv<T, P::DualSession>;
}

impl<T, P> Actionable for Send<T, P> {
    type NextAction = Self;
}

impl<T, N: Unary, P: Scoped<N>> Scoped<N> for Send<T, P> {}

impl<N: Unary, Mode, T: 'static, P: Subst<Q, N, Mode>, Q> Subst<Q, N, Mode> for Send<T, P> {
    type Substituted = Send<T, P::Substituted>;
}
