use super::*;
use bincode_crate as bincode;
use bytes::Bytes;
use serde_crate::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Bincode<O: bincode::Options>(O);

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
