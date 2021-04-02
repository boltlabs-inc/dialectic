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

use dialectic::backend::{self, By, Choice, Mut, Ref, Transmittable, Val};
use std::{any::Any, future::Future, pin::Pin};
use thiserror::Error;
use tokio::sync::mpsc;
pub use tokio::sync::mpsc::error::SendError;

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
pub struct Receiver(pub mpsc::Receiver<Box<dyn Any + Send>>);

/// A bounded sender for dynamically typed values. See [`tokio::sync::mpsc::Sender`].
#[derive(Debug, Clone)]
pub struct Sender(pub mpsc::Sender<Box<dyn Any + Send>>);

/// An unbounded receiver for dynamically typed values. See
/// [`tokio::sync::mpsc::UnboundedReceiver`].
#[derive(Debug)]
pub struct UnboundedReceiver(pub mpsc::UnboundedReceiver<Box<dyn Any + Send>>);

/// An unbounded sender for dynamically typed values. See [`tokio::sync::mpsc::UnboundedSender`].
#[derive(Debug, Clone)]
pub struct UnboundedSender(pub mpsc::UnboundedSender<Box<dyn Any + Send>>);

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
    (Sender(tx), Receiver(rx))
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

    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        <Self as backend::Transmit<Choice<LENGTH>>>::send(self, choice)
    }
}

impl<T: Send + Any> backend::Transmit<T> for Sender {
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(mpsc::Sender::send(&self.0, Box::new(message)))
    }
}

impl<T: ?Sized> backend::Transmit<T, Ref> for Sender
where
    T: Transmittable + ToOwned + Send,
    T::Owned: Send + Any + 'static,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Ref>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(mpsc::Sender::send(&self.0, Box::new(message.to_owned())))
    }
}

impl<T: ?Sized> backend::Transmit<T, Mut> for Sender
where
    T: Transmittable + ToOwned + Send,
    T::Owned: Send + Any + 'static,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Mut>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(mpsc::Sender::send(&self.0, Box::new(message.to_owned())))
    }
}

impl backend::Receiver for Receiver {
    type Error = RecvError;

    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        <Self as backend::Receive<Choice<LENGTH>>>::recv(self)
    }
}

impl<T: Send + Any> backend::Receive<T> for Receiver {
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move {
            match mpsc::Receiver::recv(&mut self.0).await {
                None => Err(RecvError::Closed),
                Some(b) => match b.downcast() {
                    Err(b) => Err(RecvError::DowncastFailed(b)),
                    Ok(t) => Ok(*t),
                },
            }
        })
    }
}

impl backend::Transmitter for UnboundedSender {
    type Error = SendError<Box<dyn Any + Send>>;

    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        <Self as backend::Transmit<Choice<LENGTH>>>::send(self, choice)
    }
}

impl<T: Send + Any> backend::Transmit<T> for UnboundedSender {
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move { mpsc::UnboundedSender::send(&self.0, Box::new(message)) })
    }
}

impl<T: ?Sized> backend::Transmit<T, Ref> for UnboundedSender
where
    T: Transmittable + ToOwned + Send + Sync,
    T::Owned: Send + Any + 'static,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Ref>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move { mpsc::UnboundedSender::send(&self.0, Box::new(message.to_owned())) })
    }
}

impl<T: ?Sized> backend::Transmit<T, Mut> for UnboundedSender
where
    T: Transmittable + ToOwned + Send,
    T::Owned: Send + Any + 'static,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Mut>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move { mpsc::UnboundedSender::send(&self.0, Box::new(message.to_owned())) })
    }
}

impl backend::Receiver for UnboundedReceiver {
    type Error = RecvError;

    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        <Self as backend::Receive<Choice<LENGTH>>>::recv(self)
    }
}

impl<T: Send + Any> backend::Receive<T> for UnboundedReceiver {
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move {
            match mpsc::UnboundedReceiver::recv(&mut self.0).await {
                None => Err(RecvError::Closed),
                Some(b) => match b.downcast() {
                    Err(b) => Err(RecvError::DowncastFailed(b)),
                    Ok(t) => Ok(*t),
                },
            }
        })
    }
}
