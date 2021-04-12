[![Rust](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml/badge.svg)](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml)
![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
[![crates.io](https://img.shields.io/crates/v/dialectic-tokio-mpsc)](https://crates.io/crates/dialectic-tokio-mpsc)
[![docs.rs documentation](https://docs.rs/dialectic-tokio-mpsc/badge.svg)](https://docs.rs/dialectic-tokio-mpsc)

This crate contains the Tokio/MPSC backend for [Dialectic](https://crates.io/crates/dialectic). It
supports send/receive operations over all types which are `Send + Any`. This is useful for
inter-task and inter-thread communication, especially if you're working on a protocol which needs to
be tested locally and is written to be backend-agnostic. There are a few important types:

- The `dialectic_tokio_mpsc::Chan<P>` synonym is a quick type synonym for a channel which uses a
  bounded MPSC `Sender`/`Receiver` pair, and analogously the
  `dialectic_tokio_mpsc::UnboundedChan<P>` provides a similar functionality for unbounded MPSC channels.
- The `dialectic_tokio_mpsc::Receiver`/`Sender`/`UnboundedReceiver`/`UnboundedSender` types
  transparently wrap the underlying MPSC receiver/sender types. If not for the orphan rules,
  Dialectic's `Transmitter`/`Receiver`/`Transmit`/`Receive` *traits* would be directly implemented
  on the `tokio::mpsc` types, but Rust does not allow that.
- Addditionally, `dialectic_tokio_mpsc::channel()` and `unbounded_channel()` are provided for
  conveniently constructing pairs of these transport types.