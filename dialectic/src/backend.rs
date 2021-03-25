//! The interface implemented by all transport backends for a [`Chan`](crate::Chan).
//!
//! A [`Chan<S, Tx, Rx>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order to use a `Chan` to run a session, these underlying channels
//! must implement the traits [`Transmitter`] and [`Receiver`], as well as [`Transmit<T>`](Transmit)
//! and [`Receive<T>`](Receive) for at least the types `T` used in those capacities in any given
//! session.
//!
//! Functions which are generic over their backend will in turn need to specify the bounds
//! `Transmit<T>` and `Receive<T>` for all `T`s they send and receive, respectively. The
//! [`Transmitter`](macro@crate::Transmitter) and [`Receiver`](macro@crate::Receiver) attribute
//! macros make this bound specification succinct; see their documentation for more details.

#[doc(no_inline)]
pub use call_by::{By, Convention, Mut, Ref, Val};
use std::{future::Future, pin::Pin};

mod choice;
pub use choice::*;

/// A backend transport used for transmitting (i.e. the `Tx` parameter of [`Chan`](crate::Chan))
/// must implement [`Transmitter`], which specifies what type of errors it might return and whether
/// it sends things by owned value or by reference, as well as giving a method to send [`Choice`]s
/// across the channel. This is a super-trait of [`Transmit`], which is what's actually needed to
/// receive particular values over a [`Chan`](crate::Chan).
///
/// If you're writing a function and need a lot of different `Transmit<T>` bounds, the
/// [`Transmitter`](macro@crate::Transmitter) attribute macro can help you specify them more
/// succinctly.
pub trait Transmitter {
    /// The type of possible errors when sending.
    type Error;

    /// The calling convention for sending values over this channel. Possible options: [`Val`],
    /// [`Ref`], or [`Mut`]. For backends that move owned values, pick [`Val`]; for those which
    /// serialize by taking references, pick [`Ref`]. [`Mut`] is an unusual choice of calling
    /// convention, but is supported.
    type Convention: Convention;

    /// Send any `Choice<N>` using the [`Convention`] specified by the trait implementation.
    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>;
}

/// If a transport is `Transmit<T>`, we can use it to [`send`](Transmit::send) a message of type `T`
/// by [`Val`] or [`Ref`], depending on the calling convention specified by its [`Transmitter`]
/// implementation.
///
/// If you're writing a function and need a lot of different `Transmit<T>` bounds, the
/// [`Transmitter`](macro@crate::Transmitter) attribute macro can help you specify them more
/// succinctly.
///
/// # Examples
///
/// For an example of implementing [`Transmit`], check out the source for the implementation of
/// [`Transmit`] for the [`dialectic_tokio_mpsc::Sender`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
///
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
/// implement [`Receiver`], which specifies what type of errors it might return. This is a
/// super-trait of [`Receive`], which is what's actually needed to receive particular values over a
/// [`Chan`](crate::Chan).
///
/// If you're writing a function and need a lot of different `Receive<T>` bounds, the
/// [`Receiver`](macro@crate::Receiver) attribute macro can help you specify them more succinctly.
pub trait Receiver {
    /// The type of possible errors when receiving.
    type Error;

    /// Receive any `Choice<N>`. It is impossible to construct a `Choice<0>`, so if `N = 0`, a
    /// [`Receiver::Error`] must be returned.
    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>;
}

/// If a transport is `Receive<T>`, we can use it to [`recv`](Receive::recv) a message of type `T`.
///
/// If you're writing a function and need a lot of different `Receive<T>` bounds, the
/// [`Receiver`](macro@crate::Receiver) attribute macro can help you specify them more succinctly.
///
/// # Examples
///
/// For an example of implementing [`Receive`], check out the source for the implementation of
/// [`Receive`] for the [`dialectic_tokio_mpsc::Receiver`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
///
/// [`dialectic_tokio_mpsc::Receiver`]:
/// https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Receiver.html
pub trait Receive<T>: Receiver {
    /// Receive a message. This may require type annotations for disambiguation.
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>>;
}
