[![Rust](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml/badge.svg)](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml)
![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic-null)
[![crates.io](https://img.shields.io/crates/v/dialectic-null)](https://crates.io/crates/dialectic-null)
[![docs.rs documentation](https://docs.rs/dialectic-null/badge.svg)](https://docs.rs/dialectic-null)

This crate contains the "null" backend for [Dialectic](https://crates.io/crates/dialectic). If you
are a user, you will likely never have a use for this, as what it does is completely eliminate any
transport backend *and* it can only send and receive the unit type `()`. The null backend is used
within Dialectic for testing and benchmarking purposes, for example to determine how much overhead
Dialectic's constructs have over raw operations.