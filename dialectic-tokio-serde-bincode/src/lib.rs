//! This crate provides an implementation of the [`bincode`] serialization format compatible with
//! the [`dialectic-tokio-serde`] backend for the [`dialectic`] crate. To use it, you will also need
//! to import the [`dialectic-tokio-serde`] crate.
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]

use bytes::Bytes;
use dialectic_tokio_serde::*;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_util::codec::length_delimited::LengthDelimitedCodec;

/// The [Bincode](bincode) binary serialization format.
///
/// To construct with default options, use `Bincode::default()`. To configure custom options, use
/// the Bincode crate's [bincode::Options] builder, then the [`.into()`](Into::into) method to
/// construct a [`Bincode`].
#[derive(Debug, Clone, Copy, Default)]
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
/// [`LengthDelimitedCodec`](tokio_util::codec::LengthDelimitedCodec) for framing, returning a
/// symmetrical pair of [`Sender`] and [`Receiver`].
///
/// The `length_field_bytes` parameter indicates how many bytes to use for representing the length
/// of each frame on the wire, and must range between 1 and 8, inclusive.
///
/// The `max_length` parameter indicates the maximum length of any message received or sent. An
/// error is thrown during encoding and decoding if a message would exceed this length.
pub fn length_delimited<W: AsyncWrite, R: AsyncRead>(
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
