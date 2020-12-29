use super::sealed::IsSession;
use super::*;

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

impl<T, P> IsSession for Send<T, P> {}

impl<T, P: Session> Session for Send<T, P> {
    type Dual = Recv<T, P::Dual>;
}

impl<N: Unary, T, P: Scoped<N>> Scoped<N> for Send<T, P> {}

impl<E, T, P> Actionable<E> for Send<T, P>
where
    P: Scoped<E::Depth>,
    E: Environment,
    E::Dual: Environment,
{
    type Action = Send<T, P>;
    type Env = E;
}
