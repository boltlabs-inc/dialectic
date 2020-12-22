use std::{future::Future, pin::Pin};

use crate::backend::{Receive, Ref, Transmit, Val};
use bytes::{Bytes, BytesMut};
use futures::sink::SinkExt;
use futures::stream::StreamExt;
use std::fmt::{Debug, Display};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_serde::{Deserializer, Serializer};
use tokio_util::codec::{Decoder, Encoder, FramedRead, FramedWrite};

pub fn symmetrical<F, E, W, R>(
    format: F,
    encoding: E,
    writer: W,
    reader: R,
) -> (Sender<F, E, W>, Receiver<F, E, R>)
where
    F: Clone,
    E: Decoder + Clone,
    W: AsyncWrite,
    R: AsyncRead,
{
    (
        Sender::new(format.clone(), encoding.clone(), writer),
        Receiver::new(format, encoding, reader),
    )
}

#[derive(Debug)]
pub struct Sender<F, E, W> {
    serializer: F,
    framed_write: FramedWrite<W, E>,
}

impl<F, E, W: AsyncWrite> Sender<F, E, W> {
    pub fn new(serializer: F, encoder: E, writer: W) -> Self {
        Sender {
            serializer,
            framed_write: FramedWrite::new(writer, encoder),
        }
    }
}

impl<'a, T: Sync + 'a, F, E, W> Transmit<'a, T, Ref> for Sender<F, E, W>
where
    F: Serializer<T> + Unpin + Send,
    F::Error: Send,
    E: Encoder<Bytes> + Send,
    W: AsyncWrite + Unpin + Send,
{
    type Error = SendError<T, F, E>;

    fn send<'async_lifetime>(
        &'async_lifetime mut self,
        message: &'a T,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move {
            let serialized = Pin::new(&mut self.serializer)
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
    T: Send,
    F: Serializer<T> + Unpin + Send,
    E: Encoder<Bytes> + Send,
    W: AsyncWrite + Unpin + Send,
{
    type Error = SendError<T, F, E>;

    fn send<'async_lifetime>(
        &'async_lifetime mut self,
        message: T,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        'a: 'async_lifetime,
    {
        Box::pin(async move {
            let serialized = Pin::new(&mut self.serializer)
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

impl<F, D: Decoder, R: AsyncRead> Receiver<F, D, R> {
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
    F: Deserializer<T> + Unpin + Send,
    D: Decoder<Item = BytesMut> + Send,
    R: AsyncRead + Unpin + Send,
{
    type Error = RecvError<T, F, D>;

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
            Ok(Pin::new(&mut self.deserializer)
                .deserialize(&unframed)
                .map_err(RecvError::Deserialize)?)
        })
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SendError<T, F: Serializer<T>, E: Encoder<Bytes>> {
    Serialize(F::Error),
    Encode(E::Error),
}

impl<T, F: Serializer<T>, E: Encoder<Bytes>> Debug for SendError<T, F, E>
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

impl<T, F: Serializer<T>, E: Encoder<Bytes>> Display for SendError<T, F, E>
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

impl<T, F, E> std::error::Error for SendError<T, F, E>
where
    F: Serializer<T>,
    E: Encoder<Bytes>,
    F::Error: Display + Debug,
    E::Error: Display + Debug,
{
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RecvError<T, F: Deserializer<T>, D: Decoder> {
    Deserialize(F::Error),
    Decode(D::Error),
    Closed,
}

impl<T, F: Deserializer<T>, D: Decoder> Debug for RecvError<T, F, D>
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

impl<T, F: Deserializer<T>, D: Decoder> Display for RecvError<T, F, D>
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

impl<T, F, D> std::error::Error for RecvError<T, F, D>
where
    F: Deserializer<T>,
    D: Decoder,
    F::Error: Display + Debug,
    D::Error: Display + Debug,
{
}
