use super::sealed::IsSession;
use crate::prelude::*;
use std::{any::Any, marker::PhantomData};

/// Send a message of type `T` using [`send`](crate::CanonicalChan::send), then continue with
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

impl<T: Any, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

impl<N: Unary, T: Any, P: Scoped<N>> Scoped<N> for Send<T, P> {}

impl<E, T: Any, P> Actionable<E> for Send<T, P>
where
    P: Scoped<E::Depth>,
    E: Environment,
{
    type Action = Send<T, P>;
    type Env = E;
}
