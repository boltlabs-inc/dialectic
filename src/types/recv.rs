use super::sealed::IsSession;
use crate::prelude::*;
use std::{any::Any, marker::PhantomData};

/// Receive a message of type `T` using [`recv`](crate::CanonicalChan::recv), then continue with
/// protocol `P`.
///
/// # Notes
///
/// A session ending with a `Recv` can be abbreviated: `Recv<String>` is shorthand for `Recv<String,
/// Done>`.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Recv<T, P = Done>(pub PhantomData<T>, pub P);

impl<T: Any, P: Any> IsSession for Recv<T, P> {}

impl<T: Any, P: HasDual> HasDual for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

impl<T, P> Actionable for Recv<T, P> {
    type Action = Self;
}

impl<T, N: Unary, P: Scoped<N>> Scoped<N> for Recv<T, P> {}

impl<N: Unary, Mode, T: 'static, P: Subst<Q, N, Mode>, Q> Subst<Q, N, Mode> for Recv<T, P> {
    type Substituted = Recv<T, P::Substituted>;
}
