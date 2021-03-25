A Tokio-compatible backend which can be used with any transport that supports Tokio's
`AsyncWrite`/`AsyncRead` traits, and can send/receive any types which implement serde's
`Serialize`/`Deserialize` traits. This backend serializes its data using the [`bincode` format.](https://crates.io/crates/bincode)