A Tokio-compatible [Dialectic](https://crates.io/crates/dialectic) backend plugin for the
[`dialectic-tokio-serde`](https://crates.io/crates/dialectic-tokio-serde) crate. It can be used with
any transport that supports Tokio's `AsyncWrite`/`AsyncRead` traits, and can send/receive any types
which implement serde's `Serialize`/`Deserialize` traits. This backend serializes its data using the
[`bincode` format.](https://crates.io/crates/bincode)