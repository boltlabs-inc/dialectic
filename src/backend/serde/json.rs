use super::*;
use serde_json as json;
use tokio_util::codec::LinesCodec;

/// The [JSON](json) serialization format.
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    /// **Important:** This is *not compatible* with line-delimited codecs, because its output
    /// will contain newlines.
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

/// Pairs the [`Json`] serialization format with the [`LinesCodec`](super::codec::LinesCodec) for
/// framing, returning a symmetrical pair of [`Sender`] and [`Receiver`].
///
/// The `max_length` parameter indicates the maximum length of any message received or sent. An
/// error is thrown during encoding and decoding if a message exceeds this length.
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
pub fn json_lines<W: AsyncWrite, R: AsyncRead>(
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
