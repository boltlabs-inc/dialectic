//! This crate provides a backend implementation for the [`dialectic`] crate using
//! [`tokio::sync::mpsc`] channels carrying boxed values `Box<dyn Any
//! + Send>`, which are downcast to their true type (inferred from the session type) on the other
//!   end of the channel. Select this backend if you're using the Tokio runtime for asynchrony and
//!   you only need to communicate between tasks in the same process.

#![allow(clippy::type_complexity)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]

use dialectic::backend::{self, By, Choice, Val};
use futures::ready;
use std::{
    any::Any,
    pin::Pin,
    task::{Context, Poll},
};
use thiserror::Error;
use tokio::sync::mpsc;
pub use tokio::sync::mpsc::error::SendError;
use tokio_util::sync::PollSender;

/// Shorthand for a [`Chan`](dialectic::Chan) using a bounded [`Sender`] and [`Receiver`].
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::types::Done;
/// use dialectic_tokio_mpsc as mpsc;
///
/// let _: (mpsc::Chan<Done>, mpsc::Chan<Done>) =
///     Done::channel(|| mpsc::channel(1));
/// ```
pub type Chan<P> = dialectic::Chan<P, Sender, Receiver>;

/// Shorthand for a [`Chan`](dialectic::Chan) using an unbounded [`UnboundedSender`] and
/// [`UnboundedReceiver`].
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::types::Done;
/// use dialectic_tokio_mpsc as mpsc;
///
/// let _: (mpsc::UnboundedChan<Done>, mpsc::UnboundedChan<Done>) =
///     Done::channel(mpsc::unbounded_channel);
/// ```
pub type UnboundedChan<P> = dialectic::Chan<P, UnboundedSender, UnboundedReceiver>;

/// A bounded receiver for dynamically typed values. See [`tokio::sync::mpsc::Receiver`].
#[derive(Debug)]
pub struct Receiver(mpsc::Receiver<Box<dyn Any + Send>>);

/// A bounded sender for dynamically typed values. See [`tokio::sync::mpsc::Sender`].
#[derive(Debug, Clone)]
pub struct Sender(PollSender<Box<dyn Any + Send>>);

/// An unbounded receiver for dynamically typed values. See
/// [`tokio::sync::mpsc::UnboundedReceiver`].
#[derive(Debug)]
pub struct UnboundedReceiver(mpsc::UnboundedReceiver<Box<dyn Any + Send>>);

/// An unbounded sender for dynamically typed values. See [`tokio::sync::mpsc::UnboundedSender`].
#[derive(Debug, Clone)]
pub struct UnboundedSender(mpsc::UnboundedSender<Box<dyn Any + Send>>);

/// Create a bounded mpsc channel for transporting dynamically typed values.
///
/// This is a wrapper around `tokio::sync::mpsc::channel::<Box<dyn Any + Send>>`. See
/// [`tokio::sync::mpsc::channel`].
///
/// # Examples
///
/// ```
/// let (tx, rx) = dialectic_tokio_mpsc::channel(1);
/// ```
pub fn channel(buffer: usize) -> (Sender, Receiver) {
    let (tx, rx) = mpsc::channel(buffer);
    (Sender(PollSender::new(tx)), Receiver(rx))
}

/// Create an unbounded mpsc channel for transporting dynamically typed values.
///
/// This is a wrapper around `tokio::sync::mpsc::channel::<Box<dyn Any + Send>>`. See
/// [`tokio::sync::mpsc::unbounded_channel`].
///
/// # Examples
///
/// ```
/// let (tx, rx) = dialectic_tokio_mpsc::unbounded_channel();
/// ```
pub fn unbounded_channel() -> (UnboundedSender, UnboundedReceiver) {
    let (tx, rx) = mpsc::unbounded_channel();
    (UnboundedSender(tx), UnboundedReceiver(rx))
}

/// An error thrown while receiving from or sending to a dynamically typed [`tokio::sync::mpsc`]
/// channel.
#[derive(Debug)]
pub enum Error {
    /// Error during receive.
    Recv(RecvError),
    /// Error during send.
    Send(Box<dyn Any + Send>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Recv(e) => e.fmt(fmt),
            Error::Send(_) => write!(fmt, "channel closed"),
        }
    }
}

impl std::error::Error for Error {}

impl From<RecvError> for Error {
    fn from(err: RecvError) -> Self {
        Error::Recv(err)
    }
}

impl<T: Any + Send> From<SendError<T>> for Error {
    fn from(SendError(err): SendError<T>) -> Self {
        Error::Send(Box::new(err))
    }
}

/// An error thrown while receiving from a dynamically typed [`tokio::sync::mpsc`] channel.
#[derive(Debug, Error)]
pub enum RecvError {
    /// The channel was explicitly closed, or all senders were dropped, implicitly closing it.
    #[error("channel closed")]
    Closed,
    /// A value received from the channel could not be cast into the correct expected type. This is
    /// always resultant from the other end of a channel failing to follow the session type of the
    /// channel.
    #[error("received value was not of desired type")]
    DowncastFailed(Box<dyn Any + Send>),
}

impl backend::Transmitter for Sender {
    type Error = SendError<Box<dyn Any + Send>>;
    type Convention = Val;

    fn poll_ready(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.0.poll_send_done(cx)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.poll_ready(cx)
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        ready!(self.as_mut().poll_ready(cx))?;
        self.0.close_this_sender();
        Poll::Ready(Ok(()))
    }
}

impl<T: Any + Send> backend::Transmit<T> for Sender {
    fn start_send<'a>(
        mut self: Pin<&mut Self>,
        message: <T as By<'a, Val>>::Type,
    ) -> Result<(), Self::Error>
    where
        T: By<'a, Val>,
    {
        // This transmute is safe because while Rust doesn't know that `for<'a> <T as By<'a,
        // Val>>::Type` is `T` itself, *we* know that it is. We don't have a way to ask Rust to
        // prove it during compilation, though, because we can't add the bound to the method or the
        // impl.
        let t: T = unsafe { (&message as *const _ as *const T).read() };
        std::mem::forget(message); // Prevent double-free when message would drop
        let boxed: Box<dyn Any + Send> = Box::new(t);
        self.0.start_send(boxed)
    }
}

impl backend::TransmitChoice for Sender {
    fn start_send_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        choice: Choice<LENGTH>,
    ) -> Result<(), Self::Error> {
        <Self as backend::Transmit<Choice<LENGTH>>>::start_send(self, choice)
    }
}

impl backend::Receiver for Receiver {
    type Error = RecvError;
}

impl<T: Send + Any> backend::Receive<T> for Receiver {
    fn poll_recv(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<T, Self::Error>> {
        self.0.poll_recv(cx).map(|option| match option {
            None => Err(RecvError::Closed),
            Some(boxed) => match boxed.downcast() {
                Err(boxed) => Err(RecvError::DowncastFailed(boxed)),
                Ok(t) => Ok(*t),
            },
        })
    }
}

impl backend::ReceiveChoice for Receiver {
    fn poll_recv_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<Choice<LENGTH>, Self::Error>> {
        <Self as backend::Receive<Choice<LENGTH>>>::poll_recv(self, cx)
    }
}

impl backend::Transmitter for UnboundedSender {
    type Error = SendError<Box<dyn Any + Send>>;
    type Convention = Val;

    fn poll_ready(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_close(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }
}

impl<T: Send + Any> backend::Transmit<T> for UnboundedSender {
    fn start_send<'a>(
        self: Pin<&mut Self>,
        message: <T as By<'a, Val>>::Type,
    ) -> Result<(), Self::Error>
    where
        T: By<'a, Val>,
    {
        // This transmute is safe because while Rust doesn't know that `for<'a> <T as By<'a,
        // Val>>::Type` is `T` itself, *we* know that it is. We don't have a way to ask Rust to
        // prove it during compilation, though, because we can't add the bound to the method or the
        // impl.
        let t: T = unsafe { (&message as *const _ as *const T).read() };
        std::mem::forget(message); // Prevent double-free when message would drop
        let boxed: Box<dyn Any + Send> = Box::new(t);
        self.0.send(boxed)
    }
}

impl backend::TransmitChoice for UnboundedSender {
    fn start_send_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        choice: Choice<LENGTH>,
    ) -> Result<(), Self::Error> {
        <Self as backend::Transmit<Choice<LENGTH>>>::start_send(self, choice)
    }
}

impl backend::Receiver for UnboundedReceiver {
    type Error = RecvError;
}

impl<T: Send + Any> backend::Receive<T> for UnboundedReceiver {
    fn poll_recv(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<T, Self::Error>> {
        self.0.poll_recv(cx).map(|option| match option {
            None => Err(RecvError::Closed),
            Some(boxed) => match boxed.downcast() {
                Err(boxed) => Err(RecvError::DowncastFailed(boxed)),
                Ok(t) => Ok(*t),
            },
        })
    }
}

impl backend::ReceiveChoice for UnboundedReceiver {
    fn poll_recv_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<Choice<LENGTH>, Self::Error>> {
        <Self as backend::Receive<Choice<LENGTH>>>::poll_recv(self, cx)
    }
}
