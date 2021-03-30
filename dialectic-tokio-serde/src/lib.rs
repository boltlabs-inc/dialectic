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

use dialectic::{
    backend::{self, By, Choice, Receive, ReceiveChoice, Ref, Transmit, TransmitChoice},
    Chan,
};
use futures::sink::Sink;
use futures::stream::Stream;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display};
use std::{
    pin::Pin,
    task::{Context, Poll},
};
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

impl<F, E, W: Unpin> Unpin for Sender<F, E, W> {}

impl<F, E, W> backend::Transmitter for Sender<F, E, W>
where
    F: Serializer + Send + 'static,
    E: Encoder<F::Output> + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
    F::Output: Send,
    F::Error: Send,
{
    type Error = SendError<F, E>;
    type Convention = Ref;

    fn poll_ready(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Pin::new(&mut self.framed_write)
            .poll_ready(cx)
            .map(|result| result.map_err(SendError::Encode))
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Pin::new(&mut self.framed_write)
            .poll_flush(cx)
            .map(|result| result.map_err(SendError::Encode))
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Pin::new(&mut self.framed_write)
            .poll_close(cx)
            .map(|result| result.map_err(SendError::Encode))
    }
}

impl<T, F, E, W> Transmit<T> for Sender<F, E, W>
where
    F: Serializer + Send + 'static,
    E: Encoder<F::Output> + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
    T: Serialize,
    F::Output: Send,
    F::Error: Send,
{
    fn start_send<'a>(
        mut self: Pin<&mut Self>,
        message: <T as By<'a, Ref>>::Type,
    ) -> Result<(), Self::Error>
    where
        T: By<'a, Ref>,
    {
        // This transmute is safe because while Rust doesn't know that `for<'a> <T as By<'a,
        // Ref>>::Type` is `&'a T`, *we* know that it is. We don't have a way to ask Rust to prove
        // it during compilation, though, because we can't add the bound to the method or the impl.
        let t: &T = unsafe { (&message as *const _ as *const &T).read() };
        std::mem::forget(message); // Prevent double-free when message would drop
        let serialized = self.serializer.serialize(t).map_err(SendError::Serialize)?;
        Pin::new(&mut self.framed_write)
            .start_send(serialized)
            .map_err(SendError::Encode)
    }
}

impl<F, E, W> TransmitChoice for Sender<F, E, W>
where
    F: Serializer + Send + 'static,
    E: Encoder<F::Output> + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
    F::Output: Send,
    F::Error: Send,
{
    fn start_send_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        choice: Choice<LENGTH>,
    ) -> Result<(), Self::Error> {
        <Self as backend::Transmit<Choice<LENGTH>>>::start_send(self, &choice)
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

impl<F, D, R: Unpin> Unpin for Receiver<F, D, R> {}

impl<F, D, R> backend::Receiver for Receiver<F, D, R>
where
    F: Deserializer<D::Item> + Send + 'static,
    D: Decoder + Send + 'static,
    R: AsyncRead + Unpin + Send + 'static,
{
    type Error = RecvError<F, D>;
}

impl<T, F, D, R> Receive<T> for Receiver<F, D, R>
where
    T: for<'a> Deserialize<'a>,
    F: Deserializer<D::Item> + Send + 'static,
    D: Decoder + Send + 'static,
    R: AsyncRead + Unpin + Send + 'static,
{
    fn poll_recv(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<T, Self::Error>> {
        Pin::new(&mut self.framed_read)
            .poll_next(cx)
            .map(|option| match option {
                None => Err(RecvError::Closed),
                Some(Err(e)) => Err(RecvError::Decode(e)),
                Some(Ok(serialized)) => self
                    .deserializer
                    .deserialize(&serialized)
                    .map_err(RecvError::Deserialize),
            })
    }
}

impl<F, D, R> ReceiveChoice for Receiver<F, D, R>
where
    F: Deserializer<D::Item> + Send + 'static,
    D: Decoder + Send + 'static,
    R: AsyncRead + Unpin + Send + 'static,
{
    fn poll_recv_choice<const LENGTH: usize>(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<Choice<LENGTH>, Self::Error>> {
        self.poll_recv(cx)
    }
}
