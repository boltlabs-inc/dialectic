//! This crate provides an implementation of the [`json`](serde_json) serialization format
//! compatible with the [`dialectic-tokio-serde`] backend for the [`dialectic`] crate. To use it,
//! you will also need to import the [`dialectic-tokio-serde`] crate.
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]

use dialectic_tokio_serde::*;
use serde::{Deserialize, Serialize};
use serde_json as json;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_util::codec::LinesCodec;

/// The [JSON](json) serialization format.
#[derive(Debug, Clone, Copy)]
pub struct Json {
    pretty: bool,
}

impl Json {
    /// Construct a new `Json` serialization format.
    pub fn new() -> Self {
        Json::default()
    }

    /// Construct a new `Json` serialization format which pretty-formats its output.
    ///
    /// ⚠️ **Caution:** Pretty-printed JSON serialization is **not compatible with line-delimited
    /// codecs** such as [`LinesCodec`], because pretty-printed JSON contains newlines. Combining
    /// the two will result in runtime deserialization failures.
    pub fn pretty() -> Self {
        Json { pretty: true }
    }
}

impl Default for Json {
    fn default() -> Self {
        Json { pretty: false }
    }
}

impl Serializer for Json {
    type Error = json::Error;
    type Output = String;

    fn serialize<T: Serialize>(&mut self, item: &T) -> Result<Self::Output, Self::Error> {
        json::to_string(item)
    }
}

impl<Input: AsRef<str>> Deserializer<Input> for Json {
    type Error = json::Error;

    fn deserialize<T: for<'a> Deserialize<'a>>(&mut self, src: &Input) -> Result<T, Self::Error> {
        json::from_str(src.as_ref())
    }
}

/// Pairs the [`Json`] serialization format with the [`LinesCodec`](tokio_util::codec::LinesCodec)
/// for framing, returning a symmetrical pair of [`Sender`] and [`Receiver`].
///
/// The `max_length` parameter indicates the maximum length of any message received or sent. An
/// error is thrown during encoding and decoding if a message exceeds this length.
pub fn lines<W: AsyncWrite, R: AsyncRead>(
    writer: W,
    reader: R,
    max_length: usize,
) -> (Sender<Json, LinesCodec, W>, Receiver<Json, LinesCodec, R>) {
    symmetrical(
        Json::default(),
        LinesCodec::new_with_max_length(max_length),
        writer,
        reader,
    )
}
