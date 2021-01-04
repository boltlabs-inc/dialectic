use super::sealed::IsSession;
use super::*;

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

impl<T, P> IsSession for Recv<T, P> {}

impl<T, P: Session> Session for Recv<T, P> {
    type Dual = Send<T, P::Dual>;
}

impl<N: Unary, T, P: Scoped<N>> Scoped<N> for Recv<T, P> {}

impl<E, T, P> Actionable<E> for Recv<T, P>
where
    P: Scoped<E::Depth>,
    E: Environment,
{
    type Action = Recv<T, P>;
    type Env = E;
}
