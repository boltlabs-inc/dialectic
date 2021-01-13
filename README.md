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
- gracefully handles runtime protocol violations, introducing **no panics**; and
- allows for **full duplex concurrent communication**, if specified in its type, while
  preserving all the same session-type safety guarantees.
- allows the expression of **context free sessions**, a more general form of session that than
  most other session typing libraries.

Together, these make Dialectic ideal for writing networked services that need to ensure **high
levels of availability** and **complex protocol correctness properties** in the real world,
where protocols might be violated and connections might be dropped.

