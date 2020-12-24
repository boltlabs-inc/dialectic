use super::*;
use bincode_crate as bincode;
use bytes::Bytes;
use serde_crate::{Deserialize, Serialize};
use tokio_util::codec::length_delimited::LengthDelimitedCodec;

pub use bincode::Options;

/// The [Bincode](bincode) binary serialization format.
///
/// To construct with default options, use `Bincode::default()`. To configure custom options, use
/// the Bincode crate's [Options] builder, then the [`.into()`](Into::into) method to construct a
/// [`Bincode`].
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Bincode<O: bincode::Options = bincode::DefaultOptions>(O);

impl<O: bincode::Options> From<O> for Bincode<O> {
    fn from(o: O) -> Self {
        Bincode(o)
    }
}

impl<O: bincode::Options + Clone> Serializer for Bincode<O> {
    type Error = bincode::Error;
    type Output = Bytes;

    fn serialize<T: Serialize>(&mut self, item: &T) -> Result<Self::Output, Self::Error> {
        self.0.clone().serialize(item).map(Into::into)
    }
}

impl<O: bincode::Options + Clone, Input> Deserializer<Input> for Bincode<O>
where
    Input: AsRef<[u8]>,
{
    type Error = bincode::Error;

    fn deserialize<T: for<'a> Deserialize<'a>>(&mut self, src: &Input) -> Result<T, Self::Error> {
        self.0.clone().deserialize(src.as_ref())
    }
}

/// Pairs the [`Bincode`] serialization format with the
/// [`LengthDelimitedCodec`](super::codec::LengthDelimitedCodec) for framing, returning a
/// symmetrical pair of [`Sender`] and [`Receiver`].
///
/// The `length_field_bytes` parameter indicates how many bytes to use for representing the length
/// of each frame on the wire, and must range between 1 and 8, inclusive.
///
/// The `max_length` parameter indicates the maximum length of any message received or sent. An
/// error is thrown during encoding and decoding if a message would exceed this length.
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
pub fn length_delimited_bincode<W: AsyncWrite, R: AsyncRead>(
    writer: W,
    reader: R,
    length_field_bytes: usize,
    max_length: usize,
) -> (
    Sender<Bincode, LengthDelimitedCodec, W>,
    Receiver<Bincode, LengthDelimitedCodec, R>,
) {
    let make_codec = || {
        LengthDelimitedCodec::builder()
            .max_frame_length(max_length)
            .length_field_length(length_field_bytes)
            .new_codec()
    };
    (
        Sender::new(Bincode::default(), make_codec(), writer),
        Receiver::new(Bincode::default(), make_codec(), reader),
    )
}
