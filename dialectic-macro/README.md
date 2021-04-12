[![Rust](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml/badge.svg)](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml)
![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
[![crates.io](https://img.shields.io/crates/v/dialectic-macro)](https://crates.io/crates/dialectic-macro)
[![docs.rs documentation](https://docs.rs/dialectic-macro/badge.svg)](https://docs.rs/dialectic-macro)

This crate contains the `Session!`, `Transmitter`, and `Receiver` macros for use in [Dialectic](https://crates.io/crates/dialectic), as
well as several other macros which are used internally to Dialectic. The `dialectic-macro` crate is
considered an internal implementation detail and should *never* be relied upon or depended on
outside of the `dialectic` crate itself, which re-exports all the important user-facing procedural
macros defined in this crate.

## For contributors

Internally, `dialectic-macro` is used to define large swathes of trait definitions which cover const
generics and conversions between const generics and unary type-level representations. This is due to
current limitations of const generics, and may be replaced in the future. Otherwise,
`dialectic-macro` is the main dependent of the `dialectic-compiler` crate; its functionality is used
within `dialectic-macro` to implement the `Session!` proc macro, and nowhere else.
