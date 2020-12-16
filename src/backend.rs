//! A [`Chan<Tx, Rx, P, E>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order to use a `Chan` to run a session, these underlying channels
//! must implement the traits [`Transmit`] and [`Receive`] for at least the types used in any given
//! session (and in the case of [`Transmit`], for the particular calling conventions used to pass
//! those types to [`Chan::send`](crate::Chan::send)).
//!
//! Additionally, in order to support [`offer!`](crate::Chan::offer) and
//! [`choose`](crate::Chan::choose), the sending channel `Tx` must implement `Transmit<'static, u8,
//! Val>`, and the receiving channel `Rx` must implement `Receive<u8>`.

use async_trait::async_trait;
pub use call_by::*;
use std::future::Future;

#[cfg(any(test, feature = "mpsc"))]
pub mod mpsc;

/// If something is `Transmit<'a, T, Convention>`, we can use it to [`Transmit::send`] a message of
/// type `T` by [`Val`], [`Ref`], or [`Mut`], depending on the calling convention specified.
///
/// In order to support the [`Chan::choose`](crate::Chan::choose) method, all backends must
/// implement `Transmit<'static, u8, Val>`, in addition to whatever other types and calling
/// conventions they support.
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
///
/// In order to support the [`Chan::offer`](crate::Chan::offer) method, all backends must implement
/// `Receive<u8>`, in addition to whatever other types they support.
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
