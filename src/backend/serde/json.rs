use super::*;
use serde_json as json;
use tokio_util::codec::LinesCodec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Json {
    _private: (),
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
