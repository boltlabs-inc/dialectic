//! The interface implemented by all transport backends for a [`Chan`](crate::Chan).
//!
//! A [`Chan<S, Tx, Rx>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order for `Tx` and `Rx` to serve as a backend for a
//! [`Chan`](crate::Chan), they must implement:
//!
//! - `Tx`: [`Transmitter`] + [`TransmitChoice`] + [`Transmit<T>`](Transmit) (for each `T` that is
//!   *sent* in a session on the channel)
//! - `Rx`: [`Receiver`] + [`ReceiveChoice`] + [`Receive<S>`](Receive) (for each `S` that is
//!   *received* in a session on the channel)
//!
//! Functions which are generic over their backend will in turn need to specify the bounds
//! [`Transmit<T>`](Transmit) and [`Receive<T>`](Receive) for all `T`s they send and receive,
//! respectively. The [`Transmitter`](macro@crate::Transmitter) and
//! [`Receiver`](macro@crate::Receiver) attribute macros make this bound specification succinct; see
//! their documentation for more details.
//!
//! The extension traits [`ReceiveExt`], [`TransmitterExt`], and [`TransmitExt`] provide a
//! [`recv`](ReceiveExt::recv) asynchronous method for all receivers, and
//! [`send`](TransmitExt::send), [`flush`](TransmitterExt::flush), and
//! [`close`](TransmitterExt::close) asynchronous methods for all transmitters. As the implementor
//! of a backend, you do not need to implement them yourself.

#[doc(no_inline)]
pub use call_by::{By, Convention, Mut, Ref, Val};
use std::{
    pin::Pin,
    task::{Context, Poll},
};

mod choice;
pub use choice::*;

/// A backend transport used for transmitting (i.e. the `Tx` parameter of [`Chan`](crate::Chan))
/// must implement [`Transmitter`], which specifies what type of errors it might return and whether
/// it sends things by owned value or by reference. This is a super-trait of [`Transmit`], which is
/// what's actually needed to receive particular values over a [`Chan`](crate::Chan).
///
/// If you're writing a function and need a lot of different [`Transmit<T>`](Transmit) bounds, the
/// [`Transmitter`](macro@crate::Transmitter) attribute macro can help you specify them more
/// succinctly.
pub trait Transmitter: Send + Unpin + 'static {
    /// The type of possible errors when sending.
    type Error;

    /// The calling convention for sending values over this channel. Possible options: [`Val`],
    /// [`Ref`], or [`Mut`]. For backends that move owned values, pick [`Val`]; for those which
    /// serialize by taking references, pick [`Ref`]. [`Mut`] is an unusual choice of calling
    /// convention, but is supported.
    type Convention: Convention;

    /// Attempts to prepare the transmitter to send a value.
    ///
    /// This method must be called and return `Poll::Ready(Ok(()))` prior to each call to
    /// [`Transmitter::start_send_choice`] or [`Transmit::start_send`].
    ///
    /// This method returns `Poll::Ready` once the underlying transmitter is ready to initiate a
    /// send operation. If this method returns `Poll::Pending`, the current task is registered to be
    /// notified (via `cx.wake().wake_by_ref()`) when [`poll_ready`](Transmitter::poll_ready) should
    /// be called again.
    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>>;

    /// Flush any remaining output from this transmitter.
    ///
    /// This method returns `Poll::Ready(Ok(()))` when nothing is left to be sent. If this value is
    /// returned then it is guaranteed that all previous values sent via
    /// [`Transmitter::start_send_choice`] or [`Transmit::start_send`] have been flushed, i.e. the
    /// connection is "up to date". If this method returns `Poll::Pending`, there is more work left
    /// to do, in which case the current task is registered to be notified (via
    /// `cx.wake().wake_by_ref()`) when [`poll_ready`](Transmitter::poll_ready) should be called
    /// again.

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>>;

    /// Flush any remaining output from this transmitter and attempt to close it.
    ///
    /// This method returns `Poll::Ready(Ok(()))` when nothing is left to be sent. If this value is
    /// returned then it is guaranteed that all previous values sent via
    /// [`Transmitter::start_send_choice`] or [`Transmit::start_send`] have been flushed, i.e. the
    /// connection is "up to date", and closed. If this method returns `Poll::Pending`, there is
    /// more work left to do, in which case the current task is registered to be notified (via
    /// `cx.wake().wake_by_ref()`) when [`poll_ready`](Transmitter::poll_ready) should be called
    /// again.

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>>;
}

/// A backend transport used for transmitting must implement [`TransmitChoice`], which specifies how
/// to transmit a [`Choice`] of arbitrary size.
pub trait TransmitChoice: Transmitter {
    /// Begin the process of sending any [`Choice<N>`](Choice). Each call to this function must be
    /// preceded by a successful call to [`Transmitter::poll_ready`] which returned
    /// `Poll::Ready(Ok(()))`.
    fn start_send_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        choice: Choice<LENGTH>,
    ) -> Result<(), Self::Error>;
}

/// If a transport is [`Transmit<T>`](Transmit), we can use it to [`send`](TransmitExt::send) a
/// message of type `T` by [`Val`] or [`Ref`], depending on the calling convention specified by its
/// [`Transmitter`] implementation.
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
/// [`dialectic_tokio_mpsc::Sender`]:
/// https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Sender.html
pub trait Transmit<T>: Transmitter {
    /// Begin the process of sending any `Choice<N>` using the [`Convention`] specified by the trait
    /// implementation. Each call to this function must be preceded by a successful call to
    /// [`Transmitter::poll_ready`] which returned `Poll::Ready(Ok(()))`.
    fn start_send<'a>(
        self: Pin<&mut Self>,
        message: <T as By<'a, Self::Convention>>::Type,
    ) -> Result<(), Self::Error>
    where
        T: By<'a, Self::Convention>;
}

/// A backend transport used for receiving (i.e. the `Rx` parameter of [`Chan`](crate::Chan)) must
/// implement [`Receiver`], which specifies what type of errors it might return. This is a
/// super-trait of [`Receive`], which is what's actually needed to receive particular values over a
/// [`Chan`](crate::Chan).
///
/// If you're writing a function and need a lot of different [`Receive<T>`](Receive) bounds, the
/// [`Receiver`](macro@crate::Receiver) attribute macro can help you specify them more succinctly.
pub trait Receiver: Send + Unpin + 'static {
    /// The type of possible errors when receiving.
    type Error;
}

/// A backend transport used for receiving must implement [`ReceiveChoice`], which describes how to
/// receive a [`Choice`] of arbitrary size.
pub trait ReceiveChoice: Receiver {
    /// Poll the receiver, attempting to receive a [`Choice<N>`](Choice).
    ///
    /// This method returns `Poll::Ready(Ok(choice))` when a `Choice<N>` has been received,
    /// `Poll::Pending` if the underlying receiver is not yet ready to return a result, or
    /// `Poll::Ready(Err(error))` if there was an error receiving the `Choice`.
    ///
    /// It is impossible to construct a `Choice<0>`, so if `N = 0`, a [`Receiver::Error`] must be
    /// returned.
    fn poll_recv_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<Choice<LENGTH>, Self::Error>>;
}

/// If a transport is [`Receive<T>`](Receive), we can use it to [`recv`](ReceiveExt::recv) a message
/// of type `T`.
///
/// If you're writing a function and need a lot of different [`Receive<T>`](Receive) bounds, the
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
    /// Poll the receiver, attempting to receive a `T`.
    ///
    /// This method returns `Poll::Ready(Ok(t))` when a `T` has been received, `Poll::Pending` if
    /// the underlying receiver is not yet ready to return a result, or `Poll::Ready(Err(error))` if
    /// there was an error receiving the `T`.
    fn poll_recv(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<T, Self::Error>>;
}

/// This module defines the future types returned from the methods of [`ReceiveExt`],
/// [`TransmitterExt`], and [`TransmitExt`].
pub mod futures {
    mod close;
    mod flush;
    mod receive;
    mod receive_choice;
    mod send;
    mod send_choice;

    pub use close::Close;
    pub use flush::Flush;
    pub use receive::Receive;
    pub(crate) use receive_choice::ReceiveChoice;
    pub use send::Send;
    pub(crate) use send_choice::SendChoice;
}

/// This extension trait is implemented for all [`Receive<T>`](Receive) transports, providing a
/// convenient way to receive something on the receiver in asynchronous code.
pub trait ReceiveExt<T>: Receive<T> {
    /// Asynchronously receive something on this receiver.
    ///
    /// The returned [`Receive`](futures::Receive) struct implements [`Future<Output = Result<T,
    /// Self::Error>>`](::futures::Future).
    fn recv(&mut self) -> futures::Receive<Self, T> {
        futures::Receive::new(self)
    }
}

impl<Rx: Receive<T>, T> ReceiveExt<T> for Rx {}

/// This extension trait is implemented for all [`Transmitter`]s, providing a convenient way to
/// manipulate such a transmitter in asynchronous code.
pub trait TransmitterExt: Transmitter {
    /// Asynchronously flush this transmitter, ensuring that when the returned future completes, all
    /// pending items to be sent have been transmitted in full.
    ///
    /// The returned [`Flush`](futures::Flush) struct implements [`Future<Output =
    /// Result<(), Self::Error>>`](::futures::Future).
    fn flush(&mut self) -> futures::Flush<Self> {
        futures::Flush::new(self)
    }

    /// Asynchronously close this transmitter, ensuring that when the returned future completes, all
    /// pending items to be sent have been transmitted in full, and the transmitter is closed.
    ///
    /// The returned [`Close`](futures::Close) struct implements [`Future<Output =
    /// Result<(), Self::Error>>`](::futures::Future).
    fn close(&mut self) -> futures::Close<Self> {
        futures::Close::new(self)
    }
}

impl<Tx: Transmitter> TransmitterExt for Tx {}

/// This extension trait is implemented for all [`Transmit<T>`](Transmit), providing a convenient
/// way to send values over a transmitter in asynchronous code.
pub trait TransmitExt<T>: Transmit<T> {
    /// Asynchronously send a `T` on this transmitter, by the [`Convention`] of that same
    /// transmitter. This *does not* necessarily flush the transmitter; you *must* call
    /// [`flush`](TransmitterExt::flush) to ensure that items are flushed.
    ///
    /// The returned [`SendChoice`](futures::SendChoice) struct implements [`Future<Output =
    /// Result<(), Self::Error>>`](::futures::Future).
    fn send<'a>(
        &mut self,
        message: <T as By<'a, Self::Convention>>::Type,
    ) -> futures::Send<'_, 'a, Self, T>
    where
        T: By<'a, Self::Convention>,
    {
        futures::Send::new(self, message)
    }
}

impl<Tx: Transmit<T> + Unpin, T> TransmitExt<T> for Tx {}
