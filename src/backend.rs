//! A [`Chan<Tx, Rx, P, E>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order to use a `Chan` to run a session, these underlying channels
//! must implement the traits [`Transmit`] and [`Receive`] for at least the types used in any given
//! session (and in the case of [`Transmit`], for the particular calling conventions used to pass
//! those types to [`Chan::send`](crate::Chan::send)).
//!
//! Additionally, in order to support [`offer!`](crate::Chan::offer) and
//! [`choose`](crate::Chan::choose), the sending channel `Tx` must implement `Transmit<'static, u8,
//! Val>`, and the receiving channel `Rx` must implement `Receive<u8>`.

pub use call_by::*;
use std::{future::Future, pin::Pin};

#[cfg_attr(docsrs, doc(cfg(feature = "mpsc")))]
#[cfg(any(test, feature = "mpsc"))]
pub mod mpsc;

#[cfg_attr(docsrs, doc(cfg(feature = "serialize")))]
#[cfg(any(test, feature = "serialize"))]
pub mod serde;

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

    /// Send a message using the [`CallingConvention`] specified by the trait implementation.
    fn send<'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime;
}

impl<'a, T, C, Convention> Transmit<'a, T, Convention> for &'_ mut C
where
    C: Transmit<'a, T, Convention>,
    Convention: CallingConvention,
    T: CallBy<'a, Convention>,
    <T as CallBy<'a, Convention>>::Type: Send,
{
    type Error = C::Error;

    fn send<'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        (**self).send(message)
    }
}

/// If something is `Receive<T>`, we can use it to [`Receive::recv`] a message of type `T`.
///
/// In order to support the [`Chan::offer`](crate::Chan::offer) method, all backends must implement
/// `Receive<u8>`, in addition to whatever other types they support.
pub trait Receive<T> {
    /// The type of possible errors when receiving.
    type Error;

    /// Receive a message. This may require type annotations for disambiguation.
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>>;
}

impl<T: 'static, C: Receive<T> + Send> Receive<T> for &'_ mut C {
    type Error = C::Error;

    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>> {
        (**self).recv()
    }
}
