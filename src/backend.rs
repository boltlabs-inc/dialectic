//! A [`Chan<Tx, Rx, P, E>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order to use a `Chan` to run a session, these underlying channels
//! must implement the traits [`Transmit`] and [`Receive`] for at least the types used in any given
//! session.
//!
//! Additionally, in order to support [`offer!`](crate::Chan::offer) and
//! [`choose`](crate::Chan::choose), the sending channel `Tx` must implement `Transmit<'static,
//! usize, Val>`, and the receiving channel `Rx` must implement `Receive<usize>`.

use async_trait::async_trait;
pub use call_by::*;
use std::future::Future;

pub mod mpsc;

/// If something is `Transmit<'a, T, Convention>`, we can use it to [`Transmit::send`] a message of
/// type `T` by [`Val`], [`Ref`], or [`Mut`], depending on the calling convention specified.
pub trait Transmit<'a, T, Convention: CallingConvention>
where
    T: CallBy<'a, Convention>,
    <T as CallBy<'a, Convention>>::Type: Send,
{
    /// The type of possible errors when sending.
    type Error;

    /// The type of future returned by [`Transmit::send`].
    type Future: Future<Output = Result<(), Self::Error>>;

    /// Send a message.
    fn send(&mut self, message: <T as CallBy<'a, Convention>>::Type) -> Self::Future;
}

impl<'a, T, C, Convention> Transmit<'a, T, Convention> for &'_ mut C
where
    C: Transmit<'a, T, Convention>,
    Convention: CallingConvention,
    T: CallBy<'a, Convention>,
    <T as CallBy<'a, Convention>>::Type: Send,
{
    type Error = C::Error;
    type Future = C::Future;

    fn send(&mut self, message: <T as CallBy<'a, Convention>>::Type) -> Self::Future {
        (**self).send(message)
    }
}

/// If something is `Receive<T>`, we can use it to [`Receive::recv`] a message of type `T`.
#[async_trait]
pub trait Receive<T> {
    /// The type of possible errors when receiving.
    type Error;

    /// Receive a message. This may require type annotations for disambiguation.
    async fn recv(&mut self) -> Result<T, Self::Error>;
}

#[async_trait]
impl<T: 'static, C: Receive<T> + Send> Receive<T> for &'_ mut C {
    type Error = C::Error;

    async fn recv(&mut self) -> Result<T, Self::Error> {
        (**self).recv().await
    }
}