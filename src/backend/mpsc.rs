//! A backend implementation using [`tokio::sync::mpsc`] channels carrying boxed values `Box<dyn Any
//! + Send>`, which are downcast to their true type (inferred from the session type) on the other
//! end of the channel.

use crate::*;
use async_trait::async_trait;
pub use mpsc::error::SendError;
use std::{any::Any, future::Future, pin::Pin};
use thiserror::Error;
use tokio::sync::mpsc::{self, Receiver, Sender, UnboundedReceiver, UnboundedSender};

/// Create a bounded mpsc channel for transporting dynamically typed values.
///
/// This is shorthand for `tokio::sync::mpsc::channel::<Box<dyn Any + Send>>`. See the documentation
/// for [`tokio::sync::mpsc::channel`] for more details.
pub fn channel(
    buffer: usize,
) -> (
    Sender<Box<dyn Any + std::marker::Send>>,
    Receiver<Box<dyn Any + std::marker::Send>>,
) {
    mpsc::channel(buffer)
}

/// Create an unbounded mpsc channel for transporting dynamically typed values.
///
/// This is shorthand for `tokio::sync::mpsc::channel::<Box<dyn Any + Send>>`. See the documentation
/// for [`tokio::sync::mpsc::unbounded_channel`] for more details.
pub fn unbounded_channel() -> (
    UnboundedSender<Box<dyn Any + std::marker::Send>>,
    UnboundedReceiver<Box<dyn Any + std::marker::Send>>,
) {
    mpsc::unbounded_channel()
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
    DowncastFailed(Box<dyn Any + std::marker::Send>),
}

impl<'a, 'b, T: std::marker::Send + Any> Transmit<'a, T, Val>
    for Sender<Box<dyn Any + std::marker::Send + 'b>>
{
    type Error = SendError<T>;
    type Future = Pin<Box<dyn Future<Output = Result<(), SendError<T>>> + std::marker::Send>>;

    fn send(&mut self, message: T) -> Self::Future {
        let this = self.clone();
        Box::pin(async move {
            Sender::send(&this, Box::new(message)).await.map_err(
                |SendError(message): SendError<Box<dyn Any + std::marker::Send>>| {
                    SendError(*message.downcast().unwrap())
                },
            )
        })
    }
}

#[async_trait]
impl<'a, T: std::marker::Send + Any> Receive<T>
    for Receiver<Box<dyn Any + std::marker::Send + 'a>>
{
    type Error = RecvError;

    async fn recv(&mut self) -> Result<T, Self::Error> {
        match Receiver::recv(self).await {
            None => Err(RecvError::Closed),
            Some(b) => match b.downcast() {
                Err(b) => Err(RecvError::DowncastFailed(b)),
                Ok(t) => Ok(*t),
            },
        }
    }
}

impl<'a, 'b, T: std::marker::Send + Any> Transmit<'a, T, Val>
    for UnboundedSender<Box<dyn Any + std::marker::Send + 'b>>
{
    type Error = SendError<T>;
    type Future = Pin<Box<dyn Future<Output = Result<(), SendError<T>>> + std::marker::Send>>;

    fn send(&mut self, message: T) -> Self::Future {
        let this = self.clone();
        Box::pin(async move {
            UnboundedSender::send(&this, Box::new(message)).map_err(
                |SendError(message): SendError<Box<dyn Any + std::marker::Send>>| {
                    SendError(*message.downcast().unwrap())
                },
            )
        })
    }
}

#[async_trait]
impl<'a, T: std::marker::Send + Any> Receive<T>
    for UnboundedReceiver<Box<dyn Any + std::marker::Send + 'a>>
{
    type Error = RecvError;

    async fn recv(&mut self) -> Result<T, Self::Error> {
        match UnboundedReceiver::recv(self).await {
            None => Err(RecvError::Closed),
            Some(b) => match b.downcast() {
                Err(b) => Err(RecvError::DowncastFailed(b)),
                Ok(t) => Ok(*t),
            },
        }
    }
}
