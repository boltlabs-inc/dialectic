//! This crate provides an interface to build a family of backend implementations using the
//! [`serde`](serde) crate to transport [`Serialize`] and [`Deserialize`] values by reference across
//! any [`AsyncRead`] and [`AsyncWrite`] transports. In order to use it, you will need to depend on
//! some other crate which provides the definition of a serialization format; none such are defined
//! here.
//!
//! To use this backend, select:
//! - a particular serialization format, such as from the [`dialectic_tokio_serde_bincode`] or
//!   [`dialectic_tokio_serde_json`] crates,
//! - a particular [`codec`] for encoding and decoding frames, and
//! - your choice of [`AsyncRead`] and [`AsyncWrite`] reader and writer.
//!
//! Then, use [`symmetrical`](symmetrical)([`_with_capacity`](symmetrical_with_capacity)) to
//! construct a pair of [`Sender`] and [`Receiver`].
//!
//! If your outgoing and incoming streams are encoded or serialized differently, or your
//! serialization or encoding format is not [`Clone`], use [`Sender::new`] and [`Receiver::new`]
//! directly to construct each end of the connection.
//!
//! [`dialectic_tokio_serde_bincode`]: https://docs.rs/dialectic-tokio-serde-bincode
//! [`dialectic_tokio_serde_json`]: https://docs.rs/dialectic-tokio-serde-json

#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]

use std::{future::Future, pin::Pin};

use dialectic::{
    backend::{self, Choice, Receive, Ref, Transmit},
    call_by::By,
    Chan,
};
use futures::sink::SinkExt;
use futures::stream::StreamExt;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_util::codec::{Decoder, Encoder, FramedRead, FramedWrite};

mod error;
pub use error::*;

#[doc(no_inline)]
pub use tokio_util::codec;

/// The serialization end of a serialization format: an object which can serialize any [`Serialize`]
/// value.
///
/// This trait *resembles* [`serde::Serializer`](serde::Serializer), but is not identical to
/// it. Unlike [`serde::Serializer`](serde::Serializer), it defines the
/// [`Output`](Serializer::Output) of a serializer, which should be something like `Bytes`,
/// [`String`], or another output format.
///
/// Most [`serde::Serializer`](serde::Serializer)s can be easily given an instance of
/// [`Serializer`]. When implementing this trait, you should pick the most specific output format
/// for [`Output`](Serializer::Output). For instance, if you can serialize to a [`String`] or a
/// [`Vec<u8>`](Vec), pick [`String`], because it implements [`AsRef<[u8]>`](AsRef).
pub trait Serializer {
    /// The type of errors during serialization.
    type Error;

    /// The output format for serialization (e.g. `Bytes`, `String`, etc.).
    type Output;

    /// Serialize a reference to any [`Serialize`] value.
    fn serialize<T: Serialize>(&mut self, item: &T) -> Result<Self::Output, Self::Error>;
}

/// The deserialization end of a serialization format: an object which can deserialize to any
/// non-lifetime-restricted [`Deserialize`] value.
///
/// This trait *resembles* [`serde::Deserializer`](serde::Deserializer), but is not identical
/// to it. Unlike [`serde::Deserializer`](serde::Deserializer), it is parameterized by the
/// `Input` to a deserializer, which might be something like `Bytes`, [`String`], or another input
/// format.
///
/// Most [`serde::Deserializer`](serde::Deserializer)s can be easily given an instance of
/// [`Deserializer`]. When implementing this trait, you should usually be as general as possible for
/// the `Input` parameter. Consider whether you can implement it for all [`AsRef<[u8]>`](AsRef) or
/// [`AsRef<str>`](AsRef) rather than a single concrete input format.
pub trait Deserializer<Input> {
    /// The type of errors during deserialization.
    type Error;

    /// Deserialize any [`Deserialize`] value from the input format (e.g. `Bytes`, `String`, etc.),
    /// provided that the deserialized value can live forever.
    fn deserialize<T: for<'a> Deserialize<'a>>(&mut self, src: &Input) -> Result<T, Self::Error>;
}

/// Create a [`Sender`]/[`Receiver`] pair which use the same serialization format and frame encoding
/// in both directions.
pub fn symmetrical<F, E, W, R>(
    format: F,
    encoding: E,
    writer: W,
    reader: R,
) -> (Sender<F, E, W>, Receiver<F, E, R>)
where
    F: Serializer + Deserializer<<E as Decoder>::Item> + Clone,
    E: Encoder<<F as Serializer>::Output> + Decoder + Clone,
    W: AsyncWrite,
    R: AsyncRead,
{
    (
        Sender::new(format.clone(), encoding.clone(), writer),
        Receiver::new(format, encoding, reader),
    )
}

/// A [`Chan`] for the session type `S` and the environment `E`, using a symmetrical
/// serialization/encoding and the [`AsyncWrite`]/[`AsyncRead`] pair `W`/`R` as transport.
pub type SymmetricalChan<S, F, E, W, R> = Chan<S, Sender<F, E, W>, Receiver<F, E, R>>;

/// Create a [`Sender`]/[`Receiver`] pair which use the same serialization format and frame encoding
/// in both directions, allocating an initial capacity for the read buffer on the receiver.
pub fn symmetrical_with_capacity<F, E, W, R>(
    format: F,
    encoding: E,
    writer: W,
    reader: R,
    capacity: usize,
) -> (Sender<F, E, W>, Receiver<F, E, R>)
where
    F: Serializer + Deserializer<<E as Decoder>::Item> + Clone,
    E: Encoder<<F as Serializer>::Output> + Decoder + Clone,
    W: AsyncWrite,
    R: AsyncRead,
{
    (
        Sender::new(format.clone(), encoding.clone(), writer),
        Receiver::with_capacity(format, encoding, reader, capacity),
    )
}

/// A `Sender<F, E, W>` is capable of sending any [`Serialize`] value using the serialization format
/// `F` and the frame encoding `E` to the asynchronous writer `W`.
#[derive(Debug)]
pub struct Sender<F, E, W> {
    serializer: F,
    framed_write: FramedWrite<W, E>,
}

impl<F: Serializer, E: Encoder<F::Output>, W: AsyncWrite> Sender<F, E, W> {
    /// Construct a new [`Sender`] given a [`Serializer`], [`Encoder`], and [`AsyncWrite`]r.
    pub fn new(serializer: F, encoder: E, writer: W) -> Self {
        Sender {
            serializer,
            framed_write: FramedWrite::new(writer, encoder),
        }
    }
}

impl<F, E, W> backend::Transmitter for Sender<F, E, W>
where
    F: Serializer + Unpin + Send,
    F::Output: Send,
    F::Error: Send,
    E: Encoder<F::Output> + Send,
    W: AsyncWrite + Unpin + Send,
{
    type Error = SendError<F, E>;
    type Convention = Ref;

    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move {
            let serialized = self
                .serializer
                .serialize(&choice)
                .map_err(SendError::Serialize)?;
            self.framed_write
                .send(serialized)
                .await
                .map_err(SendError::Encode)?;
            Ok(())
        })
    }
}

impl<T, F, E, W> Transmit<T> for Sender<F, E, W>
where
    T: Serialize + Sync,
    F: Serializer + Unpin + Send,
    F::Output: Send,
    F::Error: Send,
    E: Encoder<F::Output> + Send,
    W: AsyncWrite + Unpin + Send,
{
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, Ref>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move {
            let serialized = self
                .serializer
                .serialize(message)
                .map_err(SendError::Serialize)?;
            self.framed_write
                .send(serialized)
                .await
                .map_err(SendError::Encode)?;
            Ok(())
        })
    }
}

/// A `Receiver<F, D, R>` is capable of receiving any [`Deserialize`] value using the serialization
/// format `F` and the frame decoding `D`, from the asynchronous reader `R`.
#[derive(Debug)]
pub struct Receiver<F, D, R> {
    deserializer: F,
    framed_read: FramedRead<R, D>,
}

impl<F: Deserializer<D::Item>, D: Decoder, R: AsyncRead> Receiver<F, D, R> {
    /// Construct a new [`Receiver`] given a [`Deserializer`], [`Decoder`], and [`AsyncRead`]er.
    pub fn new(deserializer: F, decoder: D, reader: R) -> Self {
        Receiver {
            deserializer,
            framed_read: FramedRead::new(reader, decoder),
        }
    }

    /// Construct a new [`Receiver`] given a [`Deserializer`], [`Decoder`], and [`AsyncRead`]er,
    /// with a given initial buffer capacity.
    pub fn with_capacity(deserializer: F, decoder: D, reader: R, capacity: usize) -> Self {
        Receiver {
            deserializer,
            framed_read: FramedRead::with_capacity(reader, decoder, capacity),
        }
    }
}

impl<F, D, R> backend::Receiver for Receiver<F, D, R>
where
    F: Deserializer<D::Item> + Unpin + Send,
    D: Decoder + Send,
    R: AsyncRead + Unpin + Send,
{
    type Error = RecvError<F, D>;

    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        <Self as backend::Receive<Choice<LENGTH>>>::recv(self)
    }
}

impl<T, F, D, R> Receive<T> for Receiver<F, D, R>
where
    T: for<'a> Deserialize<'a>,
    F: Deserializer<D::Item> + Unpin + Send,
    D: Decoder + Send,
    R: AsyncRead + Unpin + Send,
{
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>> {
        Box::pin(async move {
            let unframed = self
                .framed_read
                .next()
                .await
                .ok_or(RecvError::Closed)?
                .map_err(RecvError::Decode)?;
            self.deserializer
                .deserialize(&unframed)
                .map_err(RecvError::Deserialize)
        })
    }
}
