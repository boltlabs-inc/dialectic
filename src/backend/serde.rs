use std::{future::Future, pin::Pin};

use crate::backend::{Receive, Ref, Transmit, Val};
use futures::sink::SinkExt;
use futures::stream::StreamExt;
use serde_crate::{Deserialize, Serialize};
use std::fmt::{Debug, Display};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_util::codec::{Decoder, Encoder, FramedRead, FramedWrite};

#[cfg(feature = "bincode")]
mod bincode;
#[cfg(feature = "bincode")]
pub use bincode::*;

#[cfg(feature = "json")]
mod json;
#[cfg(feature = "json")]
pub use json::*;

/// The serialization end of a serialization format: an object which can serialize any [`Serialize`]
/// value.
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
pub trait Deserializer<Input> {
    /// The type of errors during deserialization.
    type Error;

    /// Deserialize any [`Deserialize`] value from the input format (e.g. `Bytes`, `String`, etc.),
    /// provided that the deserialized value can live forever.
    fn deserialize<T: for<'a> Deserialize<'a>>(&mut self, src: &Input) -> Result<T, Self::Error>;
}

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

#[derive(Debug)]
pub struct Sender<F, E, W> {
    serializer: F,
    framed_write: FramedWrite<W, E>,
}

impl<F: Serializer, E: Encoder<F::Output>, W: AsyncWrite> Sender<F, E, W> {
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

#[derive(Debug)]
pub struct Receiver<F, D, R> {
    deserializer: F,
    framed_read: FramedRead<R, D>,
}

impl<F: Deserializer<D::Item>, D: Decoder, R: AsyncRead> Receiver<F, D, R> {
    pub fn new(deserializer: F, decoder: D, reader: R) -> Self {
        Receiver {
            deserializer,
            framed_read: FramedRead::new(reader, decoder),
        }
    }

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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SendError<F: Serializer, E: Encoder<F::Output>> {
    Serialize(F::Error),
    Encode(E::Error),
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RecvError<F: Deserializer<D::Item>, D: Decoder> {
    Deserialize(F::Error),
    Decode(D::Error),
    Closed,
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
