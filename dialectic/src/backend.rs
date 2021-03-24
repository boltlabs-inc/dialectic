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

/// A backend transport used for transmitting (i.e. the `Tx` parameter of [`Chan`](crate::Chan))
/// must implement [`Transmitter`], which specifies what type of errors it might return and whether
/// it sends things by owned value or by reference.
///
/// A valid [`Transmitter`] **must** implemented `Transmit<Choice<N>>` for all `N` from 0 through
/// 255, inclusive. Usually this is best done by a blanket instance that is generic over all `N`.
pub trait Transmitter {
    /// The type of possible errors when sending.
    type Error;

    /// The calling convention for sending values over this channel. Possible options: [`Val`],
    /// [`Ref`], or [`Mut`]. For backends that move owned values, pick [`Val`]; for those which
    /// serialize by taking references, pick [`Ref`]. [`Mut`] is an unusual choice of calling
    /// convention, but is supported.
    type Convention: Convention;
}

/// If a transport is `Transmit<T>`, we can use it to [`send`](Transmit::send) a message of type `T`
/// by [`Val`] or [`Ref`], depending on the calling convention specified by its [`Transmitter`]
/// implementation.
///
/// # Examples
///
/// For an example of implementing [`Transmit`], check out the source for the implementation of
/// [`Transmit`] for the [`dialectic_tokio_mpsc::Sender`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
/// [`dialectic_tokio_mpsc::Sender`]: https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Sender.html
pub trait Transmit<T>: Transmitter {
    /// Send a message using the [`Convention`] specified by the trait implementation.
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Self::Convention>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: By<'a, Self::Convention>,
        'a: 'async_lifetime;
}

/// A backend transport used for receiving (i.e. the `Rx` parameter of [`Chan`](crate::Chan)) must
/// implement [`Receiver`], which specifies what type of errors it might return.
pub trait Receiver {
    /// The type of possible errors when receiving.
    type Error;
}

/// If a transport is `Receive<T>`, we can use it to [`recv`](Receive::recv) a message of type `T`.
///
/// # Examples
///
/// For an example of implementing [`Receive`], check out the source for the implementation of
/// [`Receive`] for the [`dialectic_tokio_mpsc::Receiver`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
/// [`dialectic_tokio_mpsc::Receiver`]: https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Receiver.html
pub trait Receive<T>: Receiver {
    /// Receive a message. This may require type annotations for disambiguation.
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>>;
}
