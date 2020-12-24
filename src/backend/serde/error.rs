use super::*;

/// An error during operations on a [`Sender`] or [`Receiver`], unifying [`SendError`] and
/// [`RecvError`].
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
