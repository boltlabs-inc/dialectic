/*!
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

<!-- snip -->

# What now?

- If you are **new to session types** you might consider starting with the **[tutorial-style
  tour of the crate](tutorial)**.
- If you're **familiar with session types**, you might jump to the **[quick
  reference](#quick-reference)**, then read more in the [`types`](crate::types) module and the
  documentation for [`Chan`](crate::Chan).
- You may also find helpful the **[full self-contained
  examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples)**, which show how all
  the features of the crate come together to build session-typed network programs.
- If you want to **integrate your own channel type** with Dialectic, you need to implement the
  [`Transmit`] and [`Receive`] traits from the [`backend`] module.
- Or, you can **[dive into the reference documentation below](#modules)**...

# Quick reference

The [`prelude`] module exports all the relevant constructs for writing programs with Dialectic.
Most programs using Dialectic should `use dialectic::prelude::*;`.

The **[tutorial]** covers all the constructs necessary to write session-typed programs with
Dialectic. A quick summary:

- To make a pair of dual [`Chan`]s for a session type `P`: [`let (c1, c2) = P::channel(||
  {...})`](Session::channel) with some closure that builds a unidirectional underlying channel.
- To wrap an existing sender `tx` and receiver `rx` in a single [`Chan`] for `P`: [`let c =
  P::wrap(tx, rx)`](Session::wrap).
- Backend transports suitable for being wrapped in a [`Chan`] are provided in [`backend`], along
  with the [`Transmit`] and [`Receive`] traits necessary to implement your own.

Once you've got a channel, here's what you can do:

| Session Type (`S`) | Channel Operation(s) (on a channel `c: Chan<S, _, _>`) | Dual Type (`S::Dual`) |
| :----------- | :------------------- | :-------- |
| [`Send<T, P>`](Send) | Given some `t: T`, returns a new `c`:<br>[`let c = c.send(t).await?;`](Chan::send) | [`Recv<T, P::Dual>`](Recv) |
| [`Recv<T, P>`](Recv) | Returns some `t: T` and a new `c`:<br>[`let (t, c) = c.recv().await?;`](Chan::recv) | [`Send<T, P::Dual>`](Send) |
| [`Choose<Choices>`](Choose) | Given some `_N` < the length of `Choices`, returns a new `c`:<br>[`let c = c.choose(_N).await?;`](Chan::choose) | [`Offer<Choices::Duals>`](Offer) |
| [`Offer<Choices>`](Offer) | Given a set of labeled branches `_N => ...` in ascending order, exactly one for each option in the tuple `Choices`, returns a new `c` whose type each branch must match:<br>[`let c = offer!(c => { _0 => ..., _1 => ..., ... });`](offer!) | [`Choose<Choices::Duals>`](Choose) |
| [`Split<P, Q, R>`](Split) | Given a closure evaluating the session types `P` (send-only) and `Q` (receive-only) each to `Done` (potentially concurrently), returns a result and a channel for `R`:<br>[<code>let (t, c) = c.split(&#124;c&#124; async move { ... }).await?;</code>](Chan::split) | [`Split<Q::Dual, P::Dual, R::Dual>`](Split) |
| [`Loop<P>`](Loop) | Whatever operations are available for `P` | [`Loop<P::Dual>`](Loop) |
| [`Continue<N = Z>`](Continue) | Whatever operations are available for the start of the `N`th-innermost [`Loop`] | [`Continue<N>`](Continue) |
| [`Call<P, Q>`](Call) | Given a closure evaluating the session type `P` to `Done`, returns a result and a channel for the type `Q`:<br>[<code>let (t, c) = c.call(&#124;c&#124; async move { ... }).await?;</code>](Chan::call) | [`Call<P::Dual, Q::Dual>`](Call) |
| [`Done`] | Closes the channel, dropping its receive/transmit ends: [`c.close();`](Chan::close) | [`Done`] | [`c.close()`](Chan::close) |
*/

#![recursion_limit = "256"]
#![allow(clippy::type_complexity)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
#![forbid(broken_intra_doc_links)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[macro_use]
extern crate derivative;
use futures::Future;

pub mod backend;
pub mod tuple;
pub mod tutorial;
pub mod types;
pub mod unary;

mod chan;
mod error;
mod offer_macro;
mod session;

pub use chan::{Branches, Chan};
pub use dialectic_macro::Session;
pub use error::{IncompleteHalf, SessionIncomplete, Unavailable};
pub use session::Session;

use backend::*;
#[allow(unused_imports)] // For documentation linking
use prelude::*;

/// The prelude module for quickly getting started with Dialectic.
///
/// This module is designed to be imported as `use dialectic::prelude::*;`, which brings into scope
/// all the bits and pieces you need to start writing programs with Dialectic.
pub mod prelude {
    #[doc(no_inline)]
    pub use crate::backend::{Choice, Receive, Transmit};
    #[doc(no_inline)]
    pub use crate::session::Session;
    #[doc(no_inline)]
    pub use crate::tuple::{List, Tuple};
    #[doc(no_inline)]
    pub use crate::types::{Call, Choose, Continue, Done, Loop, Offer, Recv, Send, Split};
    #[doc(no_inline)]
    pub use crate::unary::constants::*;
    #[doc(no_inline)]
    pub use crate::unary::types::*;
    #[doc(no_inline)]
    pub use crate::unary::{LessThan, Unary, S, Z};
    #[doc(no_inline)]
    pub use crate::{offer, Branches, Chan, IncompleteHalf, SessionIncomplete, Unavailable};
    #[doc(no_inline)]
    pub use call_by::{CallBy, CallingConvention, Mut, Ref, Val};
    #[doc(no_inline)]
    pub use dialectic_macro::Session;
}
