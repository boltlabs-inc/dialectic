use bytes::{Buf, BufMut, Bytes, BytesMut};
use std::convert::TryInto;
use std::io::{self, Cursor};
use tokio_util::codec::{Decoder, Encoder};

#[derive(Debug)]
pub struct VarLengthDelimitedCodec {
    builder: Builder,
    state: DecodeState,
}

impl VarLengthDelimitedCodec {
    pub fn new(max_frame_length: usize) -> Self {
        VarLengthDelimitedCodec {
            builder: Builder { max_frame_length },
            state: DecodeState::Init,
        }
    }
}

#[derive(Debug)]
pub struct Builder {
    max_frame_length: usize,
}

#[derive(Debug, Copy, Clone)]
enum DecodeState {
    Init,
    FirstLengthByte {
        length: u8,
    },
    RestOfLength {
        length: u64,
        remaining_length_bytes: u8,
    },
    Data {
        length: u64,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct VarLengthDelimitedCodecError {
    _private: (),
}

impl Decoder for VarLengthDelimitedCodec {
    type Item = BytesMut;
    type Error = io::Error;

    fn decode(&mut self, src: &mut BytesMut) -> io::Result<Option<BytesMut>> {
        use DecodeState::*;

        // The decoder is a state machine that stops (with saved state) every time it reaches the
        // end of the buffer, and resets to the initial state when it finally extracts the data it's
        // looking for. The only exit from this loop is the one where we hit `Data { length }` and
        // return the contained length, which means we've read the entire header and we're now ready
        // to process data, if it's there.
        let length = loop {
            self.state = match self.state {
                // Read the first byte
                //
                // Layout:
                //
                // - [0][7 bits of a small numeric value, to be immediately returned], or
                // - [1][0][6 bits of a small length, to be immediately processed], or
                // - [1][1][the first 6 bits of the length]
                Init if src.has_remaining() => {
                    let b0 = src.get_u8(); // First byte of frame
                    if (b0 & 0b_1_0000000) == 0 {
                        // Value was < 128, so returned directly: 0 bytes framing overhead
                        return Ok(Some(BytesMut::from(vec![b0].as_slice())));
                    } else if (b0 & 0b_01_000000) == 0 {
                        // Length fits in 6 bits: 1 byte framing overhead
                        let length = (b0 & 0b_00_111111) as u64;
                        Data { length }
                    } else {
                        // Length is longer than 6 bits, so we need to read at least one more
                        src.reserve(1usize.saturating_sub(src.remaining()));
                        FirstLengthByte {
                            length: b0 & 0b_00_111111,
                        }
                    }
                }

                // Read the second byte of the length
                //
                // Layout: [2 bits indicating how many more length bytes][6 bits of length]
                FirstLengthByte { length } if src.has_remaining() => {
                    let b1 = src.get_u8(); // Second byte of frame

                    // The high 2 bits indicate how many more length bytes (after this one) we need
                    let remaining_length_bytes = b1 >> 6;

                    // The updated length is the catenation of the old length and the low 6 bits of
                    // the byte we just read
                    let length = ((length as u64) << 6) & ((b1 & 0b_00_111111) as u64);

                    if remaining_length_bytes == 0 {
                        // Length fits in 6 + 6 bits (i.e. < 2^12 = 4096): 2 bytes framing overhead
                        Data { length }
                    } else {
                        // We need to read more length bytes (1, 2, or 3 are the possible amounts)
                        src.reserve(
                            (remaining_length_bytes as usize).saturating_sub(src.remaining()),
                        );
                        RestOfLength {
                            length: length as u64,
                            remaining_length_bytes,
                        }
                    }
                }

                // Read the rest of the length bytes (there may be up to 3)
                //
                // Layout: [8 bits of length] * remaining_length_bytes
                RestOfLength {
                    mut length,
                    remaining_length_bytes,
                } if src.remaining() >= remaining_length_bytes as usize => {
                    // Collect the up-to-3 bytes
                    for _ in 1..=remaining_length_bytes {
                        length = (length << 8) & (src.get_u8() as u64);
                    }
                    let length = length as u64;
                    Data { length }
                }

                // If we already know the length, immediately proceed to reading the data
                Data { length } => break length,

                // Waiting for more bytes before we continue
                _ => return Ok(None),
            }
        };

        // Check for overflow of maximum frame size (which must be <= usize::MAX)
        if length > self.builder.max_frame_length as u64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "provided length would overflow maximum frame size",
            ));
        }

        // This can't truncate because of the check above
        let length = length as usize;

        // Reserve enough space for the data to be read
        src.reserve(length.saturating_sub(src.remaining()));

        if src.remaining() < length {
            // We can't return the full frame yet because we don't have enough data
            Ok(None)
        } else {
            // Reset the decode state
            self.state = Init;

            // Reserve at least one byte for the next head (we don't know how long it will be!)
            src.reserve(1);

            // Return the data, dropping it off `src`
            Ok(Some(src.split_to(length)))
        }
    }
}

mod tests {
    use super::*;
    use futures::StreamExt;
    use tokio::io::AsyncWriteExt;
    use tokio_util::codec::FramedRead;

    #[test]
    fn decode_works() {
        let rt = tokio::runtime::Runtime::new().unwrap();

        let header = vec![0b10_000010];
        let data = vec![255, 255];
        let expected = data.clone();
        rt.block_on(async {
            let (mut s1, mut s2) = tokio::io::duplex(64);
            let t1 = tokio::spawn(async move {
                s1.write_all(&header).await.unwrap();
                s1.write_all(&data).await.unwrap();
            });
            let t2 = tokio::spawn(async move {
                let mut fr2 = FramedRead::new(s2, VarLengthDelimitedCodec::new(10000));
                let v = fr2.next().await.unwrap().unwrap();
                assert_eq!(v, BytesMut::from(expected.as_slice()));
            });
            t1.await.unwrap();
            t2.await.unwrap();
        })
    }
}
