//! Transport backends for [`Chan`](crate::Chan), and the traits they implement in order to be used
//! as such.
//!
//! A [`Chan<S, Tx, Rx>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order to use a `Chan` to run a session, these underlying channels
//! must implement the traits [`Transmit`] and [`Receive`] for at least the types used in any given
//! session (and in the case of [`Transmit`], for the particular calling conventions used to pass
//! those types to [`Chan::send`](crate::Chan::send)).
//!
//! Additionally, in order to support [`offer!`](crate::offer) and [`choose`](crate::Chan::choose),
//! the sending channel `Tx` must implement `Transmit<Choice<N>, Val>`, and the receiving channel
//! `Rx` must implement `Receive<Choice<N>>`, for all `const N: usize`. For more information, see
//! [`Choice`](crate::Choice).

#[doc(no_inline)]
pub use call_by::*;
use std::{future::Future, pin::Pin};

mod choice;
pub use choice::*;

/// If a transport is `Transmit<T, Convention>`, we can use it to [`send`](Transmit::send) a message
/// of type `T` by [`Val`], [`Ref`], or [`Mut`], depending on the calling convention specified.
///
/// In order to support the [`Chan::choose`](crate::Chan::choose) method, all backends must
/// implement `Transmit<Choice<N>, Val>` for all `N`. For more information, see
/// [`Choice`](crate::Choice).
///
/// # Examples
///
/// For an example of implementing [`Transmit`], check out the source for the implementation of
/// [`Transmit`] for the [`dialectic_tokio_mpsc::Sender`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
/// [`dialectic_tokio_mpsc::Sender`]: https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Sender.html
pub trait Transmit<T, Convention: CallingConvention> {
    /// The type of possible errors when sending.
    type Error;

    /// Send a message using the [`CallingConvention`] specified by the trait implementation.
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: CallBy<'a, Convention>,
        <T as CallBy<'a, Convention>>::Type: Send,
        'a: 'async_lifetime;
}

/// If a transport is `Receive<T>`, we can use it to [`recv`](Receive::recv) a message of type `T`.
///
/// In order to support the [`Chan::offer`](crate::Chan::offer) method, all backends must implement
/// `Receive<Choice<N>>`, in addition to whatever other types they support. For more information,
/// see [`Choice`](crate::Choice).
///
/// # Examples
///
/// For an example of implementing [`Receive`], check out the source for the implementation of
/// [`Receive`] for the [`dialectic_tokio_mpsc::Receiver`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
/// [`dialectic_tokio_mpsc::Receiver`]: https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Receiver.html
pub trait Receive<T> {
    /// The type of possible errors when receiving.
    type Error;

    /// Receive a message. This may require type annotations for disambiguation.
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>>;
}
