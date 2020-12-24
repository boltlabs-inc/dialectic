use std::{future::Future, pin::Pin};

use crate::backend::{Receive, Ref, Transmit, Val};
use futures::sink::SinkExt;
use futures::stream::StreamExt;
use serde_crate::{Deserialize, Serialize};
use std::fmt::{Debug, Display};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_util::codec::{Decoder, Encoder, FramedRead, FramedWrite};

#[doc(no_inline)]
pub use tokio_util::codec;

#[cfg(feature = "bincode")]
mod bincode;

#[cfg(feature = "json")]
mod json;

#[cfg(any(feature = "bincode", feature = "json"))]
pub mod formats {
    //! Available formats for serialization, and helper functions for common pairings of
    //! serialization formats and framing codecs.

    #[cfg(feature = "bincode")]
    pub use super::bincode::*;

    #[cfg(feature = "json")]
    pub use super::json::*;
}

/// The serialization end of a serialization format: an object which can serialize any [`Serialize`]
/// value.
///
/// This trait *resembles* [`serde::Serializer`](serde_crate::Serializer), but is not identical to
/// it. Unlike [`serde::Serializer`](serde_crate::Serializer), it defines the
/// [`Output`](Serializer::Output) of a serializer, which should be something like `Bytes`,
/// [`String`], or another output format.
///
/// Most [`serde::Serializer`](serde_crate::Serializer)s can be easily given an instance of
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
/// This trait *resembles* [`serde::Deserializer`](serde_crate::Deserializer), but is not identical
/// to it. Unlike [`serde::Deserializer`](serde_crate::Deserializer), it is parameterized by the
/// `Input` to a deserializer, which might be something like `Bytes`, [`String`], or another input
/// format.
///
/// Most [`serde::Deserializer`](serde_crate::Deserializer)s can be easily given an instance of
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
pub fn symmetrical<F, E, I, O, W, R>(
    format: F,
    encoding: E,
    writer: W,
    reader: R,
) -> (Sender<F, E, W>, Receiver<F, E, R>)
where
    F: Serializer<Output = O> + Deserializer<I> + Clone,
    E: Encoder<O> + Decoder<Item = I> + Clone,
    W: AsyncWrite,
    R: AsyncRead,
{
    (
        Sender::new(format.clone(), encoding.clone(), writer),
        Receiver::new(format, encoding, reader),
    )
}

/// Create a [`Sender`]/[`Receiver`] pair which use the same serialization format and frame encoding
/// in both directions, allocating an initial capacity for the read buffer on the receiver.
pub fn symmetrical_with_capacity<F, E, I, O, W, R>(
    format: F,
    encoding: E,
    writer: W,
    reader: R,
    capacity: usize,
) -> (Sender<F, E, W>, Receiver<F, E, R>)
where
    F: Serializer<Output = O> + Deserializer<I> + Clone,
    E: Encoder<O> + Decoder<Item = I> + Clone,
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

impl<'a, T: 'a, F, E, W> Transmit<'a, T, Ref> for Sender<F, E, W>
where
    T: Serialize + Sync,
    F: Serializer + Unpin + Send,
    F::Output: Send,
    F::Error: Send,
    E: Encoder<F::Output> + Send,
    W: AsyncWrite + Unpin + Send,
{
    type Error = SendError<F, E>;

    fn send<'async_lifetime>(
        &'async_lifetime mut self,
        message: &'a T,
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

impl<'a, T: 'a, F, E, W> Transmit<'a, T, Val> for Sender<F, E, W>
where
    T: Serialize + Send,
    F: Serializer + Unpin + Send,
    F::Output: Send,
    F::Error: Send,
    E: Encoder<F::Output> + Send,
    W: AsyncWrite + Unpin + Send,
{
    type Error = SendError<F, E>;

    fn send<'async_lifetime>(
        &'async_lifetime mut self,
        message: T,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move {
            let serialized = self
                .serializer
                .serialize(&message)
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

impl<T, F, D, R> Receive<T> for Receiver<F, D, R>
where
    T: for<'a> Deserialize<'a>,
    F: Deserializer<D::Item> + Unpin + Send,
    D: Decoder + Send,
    R: AsyncRead + Unpin + Send,
{
    type Error = RecvError<F, D>;

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
            Ok(self
                .deserializer
                .deserialize(&unframed)
                .map_err(RecvError::Deserialize)?)
        })
    }
}

/// An error during operations on a [`Sender`] or [`Receiver`], unifying [`SendError`] and
/// [`RecvError`] into a single `enum` so that `?` can work properly in blocks of many operations on
/// channels.
pub enum Error<F: Serializer, G: Deserializer<D::Item>, E: Encoder<F::Output>, D: Decoder> {
    Send(SendError<F, E>),
    Recv(RecvError<G, D>),
}

/// An error while sending on a [`Sender`].
pub enum SendError<F: Serializer, E: Encoder<F::Output>> {
    /// An error occurred while attempting to serialize a value.
    Serialize(F::Error),
    /// An error occurred while attempting to encode and transmit a serialized value as a frame.
    Encode(E::Error),
}

/// An error while receiving from a [`Receiver`].
pub enum RecvError<F: Deserializer<D::Item>, D: Decoder> {
    /// An error occurred while attempting to deserialize a value.
    Deserialize(F::Error),
    /// An error occurred while attempting to receive and decode a serialized value as a frame.
    Decode(D::Error),
    /// The underlying stream was closed.
    Closed,
}

impl<F, G, E, D> From<SendError<F, E>> for Error<F, G, E, D>
where
    F: Serializer,
    G: Deserializer<D::Item>,
    E: Encoder<F::Output>,
    D: Decoder,
{
    fn from(err: SendError<F, E>) -> Self {
        Error::Send(err)
    }
}

impl<F, G, E, D> From<RecvError<G, D>> for Error<F, G, E, D>
where
    F: Serializer,
    G: Deserializer<D::Item>,
    E: Encoder<F::Output>,
    D: Decoder,
{
    fn from(err: RecvError<G, D>) -> Self {
        Error::Recv(err)
    }
}

impl<F, G, E, D> std::error::Error for Error<F, G, E, D>
where
    F: Serializer,
    G: Deserializer<D::Item>,
    E: Encoder<F::Output>,
    D: Decoder,
    F::Error: Debug + Display,
    G::Error: Debug + Display,
    E::Error: Debug + Display,
    D::Error: Debug + Display,
{
}

impl<F: Serializer, G: Deserializer<D::Item>, E: Encoder<F::Output>, D: Decoder> Debug
    for Error<F, G, E, D>
where
    F::Error: Debug,
    G::Error: Debug,
    E::Error: Debug,
    D::Error: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Send(err) => write!(f, "{:?}", err),
            Error::Recv(err) => write!(f, "{:?}", err),
        }
    }
}

impl<F: Serializer, G: Deserializer<D::Item>, E: Encoder<F::Output>, D: Decoder> Display
    for Error<F, G, E, D>
where
    F::Error: Display,
    G::Error: Display,
    E::Error: Display,
    D::Error: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Send(err) => write!(f, "{}", err),
            Error::Recv(err) => write!(f, "{}", err),
        }
    }
}

impl<F: Serializer, E: Encoder<F::Output>> Debug for SendError<F, E>
where
    F::Error: Debug,
    E::Error: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SendError::Serialize(err) => write!(f, "{:?}", err),
            SendError::Encode(err) => write!(f, "{:?}", err),
        }
    }
}

impl<F: Serializer, E: Encoder<F::Output>> Display for SendError<F, E>
where
    F::Error: Display,
    E::Error: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SendError::Serialize(err) => write!(f, "{}", err),
            SendError::Encode(err) => write!(f, "{}", err),
        }
    }
}

impl<F, E> std::error::Error for SendError<F, E>
where
    F: Serializer,
    E: Encoder<F::Output>,
    F::Error: Display + Debug,
    E::Error: Display + Debug,
{
}

impl<F: Deserializer<D::Item>, D: Decoder> Debug for RecvError<F, D>
where
    F::Error: Debug,
    D::Error: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RecvError::Deserialize(err) => write!(f, "{:?}", err),
            RecvError::Decode(err) => write!(f, "{:?}", err),
            RecvError::Closed => write!(f, "Closed"),
        }
    }
}

impl<F: Deserializer<D::Item>, D: Decoder> Display for RecvError<F, D>
where
    F::Error: Display,
    D::Error: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RecvError::Deserialize(err) => write!(f, "{}", err),
            RecvError::Decode(err) => write!(f, "{}", err),
            RecvError::Closed => write!(f, "connection closed"),
        }
    }
}

impl<F, D> std::error::Error for RecvError<F, D>
where
    F: Deserializer<D::Item>,
    D: Decoder,
    F::Error: Display + Debug,
    D::Error: Display + Debug,
{
}

impl<F, G, E, D> Clone for Error<F, G, E, D>
where
    F: Serializer,
    G: Deserializer<D::Item>,
    E: Encoder<F::Output>,
    D: Decoder,
    F::Error: Clone,
    G::Error: Clone,
    E::Error: Clone,
    D::Error: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Error::Send(err) => Error::Send(err.clone()),
            Error::Recv(err) => Error::Recv(err.clone()),
        }
    }
}

impl<F, E> Clone for SendError<F, E>
where
    F: Serializer,
    E: Encoder<F::Output>,
    F::Error: Clone,
    E::Error: Clone,
{
    fn clone(&self) -> Self {
        match self {
            SendError::Serialize(err) => SendError::Serialize(err.clone()),
            SendError::Encode(err) => SendError::Encode(err.clone()),
        }
    }
}

impl<F, D> Clone for RecvError<F, D>
where
    F: Deserializer<D::Item>,
    D: Decoder,
    F::Error: Clone,
    D::Error: Clone,
{
    fn clone(&self) -> Self {
        match self {
            RecvError::Deserialize(err) => RecvError::Deserialize(err.clone()),
            RecvError::Decode(err) => RecvError::Decode(err.clone()),
            RecvError::Closed => RecvError::Closed,
        }
    }
}
