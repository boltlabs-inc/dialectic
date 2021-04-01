![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
[![crates.io](https://img.shields.io/crates/v/dialectic-tokio-serde-bincode)](https://crates.io/crates/dialectic-tokio-serde-bincode)
[![docs.rs documentation](https://docs.rs/dialectic-tokio-serde-bincode/badge.svg)](https://docs.rs/dialectic-tokio-serde-bincode)

A Tokio-compatible [Dialectic](https://crates.io/crates/dialectic) backend plugin for the
[`dialectic-tokio-serde`](https://crates.io/crates/dialectic-tokio-serde) crate. It can be used with
any transport that supports Tokio's `AsyncWrite`/`AsyncRead` traits, and can send/receive any types
which implement serde's `Serialize`/`Deserialize` traits. This backend serializes its data using the
[`bincode` format.](https://crates.io/crates/bincode)