The `dialectic-tokio-serde` crate provides [Dialectic](https://crates.io/crates/dialectic)
`Transmitter` and `Receiver` types which are capable of sending/receiving any
serde-`Serialize`/`Deserialize` type. These are generic over their transport, and require three
additional parameters to function:

- Async-capable writer/reader types which implement the Tokio `AsyncWrite` and `AsyncRead` traits.
- A Tokio codec, for encoding and decoding frames which are written to/read from the asynchronous
  writer/reader.
- A `dialectic_tokio_serde::Serializer`/`Deserializer` for converting the sent/received types
  to/from the wire format. *PLEASE NOTE! These are not the serde `Serializer`/`Deserializer` traits
  but rather similar traits which also define the possible output/input types of the
  `Serializer`/`Deserializer.`*

## Tokio/serde backends for Dialectic

Currently, two formats are implemented as sister crates:

- The [`dialectic-tokio-serde-bincode`](https://crates.io/crates/dialectic-tokio-serde-bincode)
  crate, which when provided an `AsyncWrite`/`AsyncRead` transport enables serialization to/from the
  bincode format and using the `tokio_util` `LengthDelimitedCodec` for a wire encoding.
- The [`dialectic-tokio-serde-json`](https://crates.io/crates/dialectic-tokio-serde-json) crate,
  which when provided an `AsyncWrite`/`AsyncRead` transport enables serialization to/from the JSON
  format and using the `tokio_util` `LinesCodec` for a wire encoding.