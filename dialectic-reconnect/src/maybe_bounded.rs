//! This module defines two enums, [`Sender`] and [`Receiver`], which wrap either a bounded or
//! unbounded Tokio sender or receiver, respectively.
//!
//! These types implement the subset of the sender/receiver API necessary for this crate, not the
//! full API, most of which is not used.
use tokio::sync::mpsc::{
    self,
    error::{SendError, TrySendError},
};

/// A Tokio [`mpsc`] sender that could be either bounded or unbounded at runtime.
#[derive(Debug, Clone)]
pub enum Sender<T> {
    Bounded(mpsc::Sender<T>),
    Unbounded(mpsc::UnboundedSender<T>),
}

/// A Tokio [`mpsc`] receiver that could be either bounded or unbounded at runtime.
#[derive(Debug)]
pub enum Receiver<T> {
    Bounded(mpsc::Receiver<T>),
    Unbounded(mpsc::UnboundedReceiver<T>),
}

/// Create a Tokio [`mpsc`] sender/receiver pair that is either bounded or unbounded, depending on
/// whether a buffer size is specified.
pub fn channel<T>(buffer: Option<usize>) -> (Sender<T>, Receiver<T>) {
    if let Some(buffer) = buffer {
        let (tx, rx) = mpsc::channel(buffer);
        (Sender::Bounded(tx), Receiver::Bounded(rx))
    } else {
        let (tx, rx) = mpsc::unbounded_channel();
        (Sender::Unbounded(tx), Receiver::Unbounded(rx))
    }
}

impl<T> Sender<T> {
    /// Return the capacity of the underlying channel, if it is bounded, or `usize::MAX` if it is
    /// not bounded.
    pub fn capacity(&self) -> usize {
        match self {
            Sender::Bounded(tx) => tx.capacity(),
            Sender::Unbounded(_) => usize::MAX,
        }
    }

    /// Check if there is an existing receiver for this channel.
    pub fn is_closed(&self) -> bool {
        match self {
            Sender::Bounded(tx) => tx.is_closed(),
            Sender::Unbounded(tx) => tx.is_closed(),
        }
    }

    /// Try to send a message over the channel, returning an error if the channel is full or closed.
    pub fn try_send(&self, message: T) -> Result<(), TrySendError<T>> {
        match self {
            Sender::Bounded(tx) => tx.try_send(message),
            Sender::Unbounded(tx) => tx
                .send(message)
                .map_err(|SendError(t)| TrySendError::Closed(t)),
        }
    }
}

impl<T> Receiver<T> {
    /// Receive the next value for this receiver, returning `None` if all `Sender` halves have
    /// dropped.
    pub async fn recv(&mut self) -> Option<T> {
        match self {
            Receiver::Bounded(rx) => rx.recv().await,
            Receiver::Unbounded(rx) => rx.recv().await,
        }
    }
}
