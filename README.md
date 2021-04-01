# Dialectic

![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
[![crates.io](https://img.shields.io/crates/v/dialectic)](https://crates.io/crates/dialectic)
[![docs.rs documentation](https://docs.rs/dialectic/badge.svg)](https://docs.rs/dialectic)

> **dialectic (noun):** The process of arriving at the truth by stating a thesis, developing a
> contradictory antithesis, and combining them into a coherent synthesis.
>
> **dialectic (crate):** Transport-polymorphic session types for asynchronous Rust.

When two concurrent processes communicate, it's good to give their messages *types*, which
ensure every message is of an expected form.

- **Conventional types** merely describe **what is valid** to communicate.
- **Session types** describe **when it is valid** to communicate, and **in what manner**.

This crate provides a generic wrapper around almost any type of asynchronous channel that adds
compile-time guarantees that a specified *session protocol* will not be violated by any code
using the channel. Such a wrapped channel:

- has **almost no runtime cost** in time or memory;
- is **built on `async`/`.await`** to allow integration with Rust's powerful `async` ecosystem;
- gracefully handles runtime protocol violations, introducing **no panics**;
- allows for **full duplex concurrent communication**, if specified in its type, while
  preserving all the same session-type safety guarantees; and
- can even implement **context free sessions**, a more general form of session type than
  supported by most other session typing libraries.

Together, these make Dialectic ideal for writing networked services that need to ensure **high
levels of availability** and **complex protocol correctness properties** in the real world,
where protocols might be violated and connections might be dropped.

Dialectic supports a number of async runtimes and backends out-of-the-box, if you don't want to
or don't need to write your own:

- The [`dialectic-tokio-mpsc`] crate supports using Dialectic to communicate between
  tasks using Tokio's [`mpsc`] queues.
- The [`dialectic-tokio-serde`] crate supports using Dialectic to communicate over any [`AsyncRead`]/[`AsyncWrite`] transport layer encoded using any Tokio [`codec`]. A couple of Serde formats are already implemented, but it is easy to implement your own:
  - [`dialectic-tokio-serde-bincode`] backend using [`bincode`] for serialization
  - [`dialectic-tokio-serde-json`] backend using [`serde_json`] for serialization

These crates also serve as good references for writing your own backends.

## What now?

- If you are **new to session types** you might consider starting with the **[tutorial-style
  tour of the crate]**.
- If you're **familiar with session types**, you might jump to the **[quick
  reference]**, then read more about the [`Session!`] macro for specifying session types, and
  continue on to look at the [`types`] module and the documentation for [`Chan`].
- You may also find helpful the **[full self-contained
  examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples)**, which show how all
  the features of the crate come together to build session-typed network programs.
- If you want to **integrate your own channel type** with Dialectic, you need to implement the
  [`Transmit`] and [`Receive`] traits from the [`backend`] module.
- Or, you can dive into the **[reference documentation]**...

[`codec`]: https://docs.rs/tokio-util/latest/tokio_util/codec/index.html
[`mpsc`]: https://docs.rs/tokio/latest/tokio/sync/mpsc/index.html
[`AsyncRead`]: https://docs.rs/tokio/latest/tokio/io/trait.AsyncRead.html
[`AsyncWrite`]: https://docs.rs/tokio/latest/tokio/io/trait.AsyncWrite.html

[`dialectic-tokio-mpsc`]: https://crates.io/crates/dialectic-tokio-mpsc
[`dialectic-tokio-serde`]: https://crates.io/crates/dialectic-tokio-serde
[`dialectic-tokio-serde-bincode`]: https://crates.io/crates/dialectic-tokio-serde-bincode
[`dialectic-tokio-serde-json`]: https://crates.io/crates/dialectic-tokio-serde-json
[`bincode`]: https://crates.io/crates/bincode
[`serde_json`]: https://crates.io/crates/serde_json
[tutorial-style tour of the crate]: https://docs.rs/dialectic/latest/dialectic/tutorial/index.html
[quick reference]: https://docs.rs/dialectic/latest/dialectic/#quick-reference
[reference documentation]: https://docs.rs/dialectic
[`types`]: https://docs.rs/dialectic/latest/dialectic/types/index.html
[`Chan`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html
[`Transmit`]: https://docs.rs/dialectic/latest/dialectic/backend/trait.Transmit.html
[`Receive`]: https://docs.rs/dialectic/latest/dialectic/backend/trait.Receive.html
[`backend`]: https://docs.rs/dialectic/latest/dialectic/backend/index.html
[`Session!`]: https://docs.rs/dialectic/latest/dialectic/macro.Session.html
