//! A "null" backend implementation for the [`dialectic`] crate which can only send and receive the unit type `()`.
//!
//! This backend is useful primarily only for benchmarking, as it does the absolute minimum amount
//! of work, so that it is easier to isolate performance issues in Dialectic itself. You cannot
//! implement most protocols using this backend, as it is limited to transporting the unit type `()`
//! and cannot [`choose`](crate::Chan::choose) or [`offer!`](crate::offer) more than a single
//! choice.

#![allow(clippy::type_complexity)]

use dialectic::backend::*;
use dialectic::unary::{Unary, S, Z};
use std::{convert::TryInto, future::Future, pin::Pin};

/// Shorthand for a [`Chan`](crate::Chan) using a [`null`](crate::backend::null) [`Sender`] and
/// [`Receiver`].
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::backend::null;
/// use dialectic::types::Done;
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
/// let (tx, rx) = dialectic::backend::null::channel();
/// ```
pub fn channel() -> (Sender, Receiver) {
    (Sender::default(), Receiver::default())
}

/// An error thrown while receiving from or sending to a [`null`](crate::backend::null) channel.
///
/// No such errors are possible, so this type cannot be constructed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl std::error::Error for Error {}

impl Transmit<(), Val> for Sender {
    type Error = Error;

    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        _message: <() as CallBy<Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async { Ok(()) })
    }
}

impl Receive<()> for Receiver {
    type Error = Error;

    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async { Ok(()) })
    }
}

impl Transmit<Choice<S<Z>>, Val> for Sender {
    type Error = Error;

    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        _message: <Choice<S<Z>> as CallBy<Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async { Ok(()) })
    }
}

impl<N: Unary> Receive<Choice<S<N>>> for Receiver {
    type Error = Error;

    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<S<N>>, Self::Error>> + Send + 'async_lifetime>>
    {
        Box::pin(async { Ok((N::VALUE as u8).try_into().unwrap()) })
    }
}
