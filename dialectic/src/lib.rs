/*!
[![Rust](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml/badge.svg)](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml)
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
<!-- snip -->

<!-- links to other crates' docs -->
[`dialectic-tokio-mpsc`]: https://docs.rs/dialectic-tokio-mpsc
[`dialectic-tokio-serde`]: https://docs.rs/dialectic-tokio-serde
[`dialectic-tokio-serde-bincode`]: https://docs.rs/dialectic-tokio-serde-bincode
[`dialectic-tokio-serde-json`]: https://docs.rs/dialectic-tokio-serde-json
[`bincode`]: https://docs.rs/bincode
[`serde_json`]: https://docs.rs/serde_json

<!-- links to self docs -->
[tutorial-style tour of the crate]: tutorial
[quick reference]: #quick-reference
[reference documentation]: #modules
[`Session!`]: macro@Session

## Quick reference

The **[`prelude`]** module exports most of the relevant constructs for writing programs with
Dialectic. Most programs using Dialectic should `use dialectic::prelude::*;`.

The **[tutorial]** covers all the constructs necessary to write session-typed programs with
Dialectic. A quick summary:

- To specify a session type, use the [`Session!`](crate::Session@macro) macro, which defines a
  domain-specific language for session types. It compiles to session types defined in the
  [`types`](crate::types) module, which most users will never need to use directly.
- To construct a session-typed [`Chan`], use the methods on the [`Session`](crate::Session@trait)
  trait, such as [`Session::channel`] or [`Session::wrap`], depending on whether you need to create
  both sides of a channel or just one side, respectively.
- Backend transports suitable for being wrapped in a [`Chan`] are provided in [`backend`], along
  with the [`Transmit`] and [`Receive`] traits necessary to implement your own.
- When writing functions which are generic over their backend type, you will need to specify
  [`Transmit`] and [`Receive`] bounds on your backend. If you have a lot of these, the
  [`macro@Transmitter`] and [`macro@Receiver`] attribute macros can help eliminate them by letting
  you write them efficiently. All of the [examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples)
  are written to be backend-agnostic, so taking a look at them may help if you get stuck.

Once you've got a channel, here's what you can do:

| [`Session!`](crate::Session@macro) Macro Invocation | Session Type (`S`) | Channel Operation(s)<br>(on a channel `c: Chan<S, _, _>`) | Dual Type (`S::Dual`) |
| :--------------------| :----------- | :------------------- | :-------- |
| [`send T; ...`](macro@crate::Session#the-send-and-recv-keywords) | [`Send<T, P>`](Send) | Given some `t: T`, returns a new `c`:<br>[`let c = c.send(t).await?;`](Chan::send) | [`Recv<T, P::Dual>`](Recv) |
| [`recv T; ...`](macro@crate::Session#the-send-and-recv-keywords) | [`Recv<T, P>`](Recv) | Returns some `t: T` and a new `c`:<br>[`let (t, c) = c.recv().await?;`](Chan::recv) | [`Send<T, P::Dual>`](Send) |
| [`choose { 0 => ..., 1 => ..., ... }; ...`](macro@crate::Session#the-offer-and-choose-keywords) | [`Choose<Choices>`](Choose) | Given some `N` < the length of `Choices`, returns a new `c`:<br>[`let c = c.choose::<N>().await?;`](Chan::choose) | [`Offer<Choices::Duals>`](Offer) |
| [`offer { 0 => ..., 1 => ..., ... }; ...`](macro@crate::Session#the-offer-and-choose-keywords) | [`Offer<Choices>`](Offer) | Given a set of labeled branches `N => ...` in ascending order, exactly one for each option in the tuple `Choices`, returns a new `c` whose type each branch must match:<br>[`let c = offer!(c => { 0 => ..., 1 => ..., ... });`](offer!) | [`Choose<Choices::Duals>`](Choose) |
| [`loop { ... }; ...`<br>or<br>`'label: loop { ... }; ...`](macro@crate::Session#the-loop-break-and-continue-keywords) | [`Loop<P>`](Loop) | Whatever operations are available for `P` | [`Loop<P::Dual>`](Loop) |
| [`continue;`<br>or<br>`continue 'label;`](macro@crate::Session#the-loop-break-and-continue-keywords) | [`Continue<const N: usize>`](Continue) | Whatever operations are available for the start of the referred-to loop | [`Continue<N>`](Continue) |
| [`break;`<br>or<br>`break 'label;`](macro@crate::Session#the-loop-break-and-continue-keywords) | Whatever follows the `loop` | Whatever operations are available after the end of the `loop` statement | The dual of whatever follows the `loop` |
| [`call { ... }; ...`<br>or<br>`call S; ...`](macro@crate::Session#the-call-keyword) | [`Call<P, Q>`](Call) | Given a closure evaluating the session type `P` to `Done`, returns a result and a channel for the type `Q`:<br>[<code>let (t, c) = c.call(&#124;c&#124; async move { ... }).await?;</code>](Chan::call) | [`Call<P::Dual, Q::Dual>`](Call) |
| [`split { -> ..., <- ... }; ...`](macro@crate::Session#the-split-keyword) | [`Split<P, Q, R>`](Split) | Given a closure evaluating the session types `P` (send-only) and `Q` (receive-only) each to `Done` (potentially concurrently), returns a result and a channel for `R`:<br>[<code>let (t, c) = c.split(&#124;c&#124; async move { ... }).await?;</code>](Chan::split) | [`Split<Q::Dual, P::Dual, R::Dual>`](Split) |
| (empty macro invocation) | [`Done`] | Closes the channel, dropping its receive/transmit ends: [`c.close();`](Chan::close) | [`Done`] | [`c.close()`](Chan::close) |

[`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
[`dialectic_tokio_serde`]: https://docs.rs/dialectic-tokio-serde
[`dialectic_tokio_serde_bincode`]: https://docs.rs/dialectic-tokio-serde-bincode
[`dialectic_tokio_serde_json`]: https://docs.rs/dialectic-tokio-serde-json
[`AsyncWrite`]: tokio::io::AsyncWrite
[`AsyncRead`]: tokio::io::AsyncRead
[`mpsc`]: tokio::sync::mpsc
*/

#![recursion_limit = "256"]
#![allow(clippy::type_complexity)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[macro_use]
extern crate derivative;

pub mod backend;
pub mod tuple;
pub mod tutorial;
pub mod types;
pub mod unary;

mod chan;
mod error;
mod session;

pub use chan::{Branches, Chan, Over};
pub use dialectic_macro::{offer, Receiver, Session, Transmitter};
pub use error::{IncompleteHalf, SessionIncomplete, Unavailable};
pub use session::Session;

#[allow(unused_imports)] // If no backends are feature-enabled, don't warn
use backend::*;
#[allow(unused_imports)] // For documentation linking
use prelude::*;
#[allow(unused_imports)] // For documentation linking
use types::*;

/// This is a dummy module for the offer/Session proc macros to refer to from within dialectic to
/// ensure that the same path can resolve to the same types in different scopes (the scope of being
/// inside the dialectic crate somewhere and the scope of being inside a doctest being compiled
/// within dialectic, which *looks* like the dialectic crate itself to any proc macro but actually
/// isn't and has the dialectic crate as an extern crate in scope.)
pub(crate) mod dialectic {
    pub use crate::*;
}

/// The prelude module for quickly getting started with Dialectic.
///
/// This module is designed to be imported as `use dialectic::prelude::*;`, which brings into scope
/// all the bits and pieces you need to start writing programs with Dialectic.
pub mod prelude {
    #[doc(no_inline)]
    pub use crate::backend::{
        Choice, Receive, ReceiveCase, ReceiveChoice, Receiver, Transmit, TransmitCase,
        TransmitChoice, Transmitter,
    };
    #[doc(no_inline)]
    pub use crate::session::Session;
    #[doc(no_inline)]
    pub use crate::Chan;
    #[doc(no_inline)]
    pub use call_by::{Mut, Ref, Val};
    #[doc(no_inline)]
    pub use dialectic_macro::{offer, Receiver, Session, Transmitter};
}
