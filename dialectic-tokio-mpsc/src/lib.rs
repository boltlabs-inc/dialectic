//! A backend implementation using [`tokio::sync::mpsc`] channels carrying boxed values `Box<dyn Any
//! + Send>`, which are downcast to their true type (inferred from the session type) on the other
//! end of the channel.

use dialectic::backend::*;
use std::{any::Any, future::Future, pin::Pin};
use thiserror::Error;
use tokio::sync::mpsc;
pub use tokio::sync::mpsc::error::SendError;

/// Shorthand for a [`Chan`](crate::Chan) using a bounded [`mpsc`](crate::backend::mpsc) [`Sender`]
/// and [`Receiver`].
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::types::Done;
/// use dialectic::backend::mpsc;
///
/// let _: (mpsc::Chan<Done>, mpsc::Chan<Done>) =
///     Done::channel(|| mpsc::channel(1));
/// ```
pub type Chan<P> = dialectic::Chan<P, Sender, Receiver>;

/// Shorthand for a [`Chan`](crate::Chan) using an unbounded [`mpsc`](crate::backend::mpsc)
/// [`UnboundedSender`] and [`UnboundedReceiver`].
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::types::Done;
/// use dialectic::backend::mpsc;
///
/// let _: (mpsc::UnboundedChan<Done>, mpsc::UnboundedChan<Done>) =
///     Done::channel(mpsc::unbounded_channel);
/// ```
pub type UnboundedChan<P> = dialectic::Chan<P, UnboundedSender, UnboundedReceiver>;

/// A bounded receiver for dynamically typed values. See [`tokio::sync::mpsc::Receiver`].
pub struct Receiver(pub mpsc::Receiver<Box<dyn Any + Send>>);

/// A bounded sender for dynamically typed values. See [`tokio::sync::mpsc::Sender`].
pub struct Sender(pub mpsc::Sender<Box<dyn Any + Send>>);

/// An unbounded receiver for dynamically typed values. See
/// [`tokio::sync::mpsc::UnboundedReceiver`].
pub struct UnboundedReceiver(pub mpsc::UnboundedReceiver<Box<dyn Any + Send>>);

/// An unbounded sender for dynamically typed values. See [`tokio::sync::mpsc::UnboundedSender`].
pub struct UnboundedSender(pub mpsc::UnboundedSender<Box<dyn Any + Send>>);

/// Create a bounded mpsc channel for transporting dynamically typed values.
///
/// This is shorthand for `tokio::sync::mpsc::channel::<Box<dyn Any + Send>>`. See
/// [`tokio::sync::mpsc::channel`].
///
/// # Examples
///
/// ```
/// let (tx, rx) = dialectic::backend::mpsc::channel(1);
/// ```
pub fn channel(buffer: usize) -> (Sender, Receiver) {
    let (tx, rx) = mpsc::channel(buffer);
    (Sender(tx), Receiver(rx))
}

/// Create an unbounded mpsc channel for transporting dynamically typed values.
///
/// This is shorthand for `tokio::sync::mpsc::channel::<Box<dyn Any + Send>>`. See
/// [`tokio::sync::mpsc::unbounded_channel`].
///
/// # Examples
///
/// ```
/// let (tx, rx) = dialectic::backend::mpsc::unbounded_channel();
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

impl<T: Send + Any> Transmit<T, Val> for Sender {
    type Error = SendError<T>;

    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as CallBy<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move {
            mpsc::Sender::send(&self.0, Box::new(message))
                .await
                .map_err(|SendError(message): SendError<Box<dyn Any + Send>>| {
                    SendError(*message.downcast().unwrap())
                })
        })
    }
}

impl<T: Send + Any> Receive<T> for Receiver {
    type Error = RecvError;

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

impl<T: Send + Any> Transmit<T, Val> for UnboundedSender {
    type Error = SendError<T>;

    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as CallBy<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move {
            mpsc::UnboundedSender::send(&self.0, Box::new(message)).map_err(
                |SendError(message): SendError<Box<dyn Any + Send>>| {
                    SendError(*message.downcast().unwrap())
                },
            )
        })
    }
}

impl<T: Send + Any> Receive<T> for UnboundedReceiver {
    type Error = RecvError;

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
