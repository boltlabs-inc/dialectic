![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
[![crates.io](https://img.shields.io/crates/v/dialectic-tokio-serde-json)](https://crates.io/crates/dialectic-tokio-serde-json)
[![docs.rs documentation](https://docs.rs/dialectic-tokio-serde-json/badge.svg)](https://docs.rs/dialectic-tokio-serde-json)

A Tokio-compatible [Dialectic](https://crates.io/crates/dialectic) backend plugin for the
[`dialectic-tokio-serde`](https://crates.io/crates/dialectic-tokio-serde) crate. It can be used with
any transport that supports Tokio's `AsyncWrite`/`AsyncRead` traits, and can send/receive any types
which implement serde's `Serialize`/`Deserialize` traits. This backend serializes its data using
the [`serde_json` crate.](https://crates.io/crates/serde_json)