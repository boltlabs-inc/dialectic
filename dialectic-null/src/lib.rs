//! A "null" backend implementation for the [`dialectic`] crate which can only send and receive the unit type `()`.
//!
//! This backend is useful primarily only for benchmarking, as it does the absolute minimum amount
//! of work, so that it is easier to isolate performance issues in Dialectic itself. You cannot
//! implement most protocols using this backend, as it is limited to transporting the unit type `()`
//! and cannot [`choose`](dialectic::Chan::choose) or [`offer!`](dialectic::offer) more than a
//! single choice.

#![allow(clippy::type_complexity)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]

use dialectic::backend::{self, By, Choice, Val};
use std::{
    convert::TryInto,
    pin::Pin,
    result::Result,
    task::{Context, Poll},
};

/// Shorthand for a [`Chan`](dialectic::Chan) using a null [`Sender`] and [`Receiver`].
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::types::Done;
/// use dialectic_null as null;
///
/// let _: (null::Chan<Done>, null::Chan<Done>) =
///     Done::channel(null::channel);
/// ```
pub type Chan<P> = dialectic::Chan<P, Sender, Receiver>;

/// A receiver only capable of receiving the unit type `()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Receiver {
    _private: (),
}

/// A sender only capable of sending the unit type `()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Sender {
    _private: (),
}

/// Create a channel for transporting unit values `()` only.
///
/// # Examples
///
/// ```
/// let (tx, rx) = dialectic_null::channel();
/// ```
pub fn channel() -> (Sender, Receiver) {
    (Sender::default(), Receiver::default())
}

/// An error thrown while receiving from or sending to a null channel.
///
/// No such errors are possible, so this type cannot be constructed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    _private: (),
}

impl std::fmt::Display for Error {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl std::error::Error for Error {}

impl backend::Transmitter for Sender {
    type Error = Error;
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

impl backend::TransmitChoice for Sender {
    fn start_send_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        _: Choice<LENGTH>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl backend::Transmit<()> for Sender {
    fn start_send<'a>(self: Pin<&mut Self>, _: <() as By<'a, Val>>::Type) -> Result<(), Self::Error>
    where
        (): By<'a, Val>,
    {
        Ok(())
    }
}

impl backend::Receiver for Receiver {
    type Error = Error;
}

impl backend::ReceiveChoice for Receiver {
    fn poll_recv_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        _: &mut Context<'_>,
    ) -> Poll<Result<Choice<LENGTH>, Self::Error>> {
        Poll::Ready(0.try_into().map_err(|_| Error { _private: () }))
    }
}

impl backend::Receive<()> for Receiver {
    fn poll_recv(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }
}
