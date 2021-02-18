//! ![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
//! [![crates.io](https://img.shields.io/crates/v/dialectic)](https://crates.io/crates/dialectic)
//! [![docs.rs documentation](https://docs.rs/dialectic/badge.svg)](https://docs.rs/dialectic)
//!
//! > **dialectic (noun):** The process of arriving at the truth by stating a thesis, developing a
//! > contradictory antithesis, and combining them into a coherent synthesis.
//! >
//! > **dialectic (crate):** Transport-polymorphic session types for asynchronous Rust.
//!
//! When two concurrent processes communicate, it's good to give their messages *types*, which
//! ensure every message is of an expected form.
//!
//! - **Conventional types** merely describe **what is valid** to communicate.
//! - **Session types** describe **when it is valid** to communicate, and **in what manner**.
//!
//! This crate provides a generic wrapper around almost any type of asynchronous channel that adds
//! compile-time guarantees that a specified *session protocol* will not be violated by any code
//! using the channel. Such a wrapped channel:
//!
//! - has **almost no runtime cost** in time or memory;
//! - is **built on `async`/`.await`** to allow integration with Rust's powerful `async` ecosystem;
//! - gracefully handles runtime protocol violations, introducing **no panics**;
//! - allows for **full duplex concurrent communication**, if specified in its type, while
//!   preserving all the same session-type safety guarantees; and
//! - can even implement **context free sessions**, a more general form of session type than
//!   supported by most other session typing libraries.
//!
//! Together, these make Dialectic ideal for writing networked services that need to ensure **high
//! levels of availability** and **complex protocol correctness properties** in the real world,
//! where protocols might be violated and connections might be dropped.
//!
//! <!-- snip -->
//!
//! # What now?
//!
//! - If you are **new to session types** you might consider starting with the **[tutorial-style
//!   tour of the crate](tutorial)**.
//! - If you're **familiar with session types**, you might jump to the **[quick
//!   reference](#quick-reference)**, then read more in the [`types`](crate::types) module and the
//!   documentation for [`Chan`](crate::Chan).
//! - You may also find helpful the **[full self-contained
//!   examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples)**, which show how all
//!   the features of the crate come together to build session-typed network programs.
//! - If you want to **integrate your own channel type** with Dialectic, you need to implement the
//!   [`Transmit`] and [`Receive`] traits from the [`backend`] module.
//! - Or, you can **[dive into the reference documentation below](#modules)**...
//!
//! # Quick reference
//!
//! The [`prelude`] module exports all the relevant constructs for writing programs with Dialectic.
//! Most programs using Dialectic should `use dialectic::prelude::*;`.
//!
//! The **[tutorial]** covers all the constructs necessary to write session-typed programs with
//! Dialectic. A quick summary:
//!
//! - To make a pair of dual [`Chan`]s for a session type `P`: [`let (c1, c2) = P::channel(||
//!   {...})`](Session::channel) with some closure that builds a unidirectional underlying channel.
//! - To wrap an existing sender `tx` and receiver `rx` in a single [`Chan`] for `P`: [`let c =
//!   P::wrap(tx, rx)`](Session::wrap).
//! - Backend transports suitable for being wrapped in a [`Chan`] are provided in [`backend`], along
//!   with the [`Transmit`] and [`Receive`] traits necessary to implement your own.
//!
//! Once you've got a channel, here's what you can do:
//!
//! | Session Type (`S`) | Channel Operation(s) (on a channel `c: Chan<S, _, _>`) | Dual Type (`S::Dual`) |
//! | :----------- | :------------------- | :-------- |
//! | [`Send<T, P>`](Send) | Given some `t: T`, returns a new `c`:<br>[`let c = c.send(t).await?;`](Chan::send) | [`Recv<T, P::Dual>`](Recv) |
//! | [`Recv<T, P>`](Recv) | Returns some `t: T` and a new `c`:<br>[`let (t, c) = c.recv().await?;`](Chan::recv) | [`Send<T, P::Dual>`](Send) |
//! | [`Choose<Choices>`](Choose) | Given some `_N` < the length of `Choices`, returns a new `c`:<br>[`let c = c.choose(_N).await?;`](Chan::choose) | [`Offer<Choices::Duals>`](Offer) |
//! | [`Offer<Choices>`](Offer) | Given a set of labeled branches `_N => ...` in ascending order, exactly one for each option in the tuple `Choices`, returns a new `c` whose type each branch must match:<br>[`let c = offer!(c => { _0 => ..., _1 => ..., ... });`](offer!) | [`Choose<Choices::Duals>`](Choose) |
//! | [`Split<P, Q>`](Split) | Given a closure evaluating the session types `P` (send-only) and `Q` (receive-only) each to `Done` (potentially concurrently), returns a result and a channel for `Done`:<br>[<code>let (t, c) = c.split(&#124;c&#124; async move { ... }).await?;</code>](Chan::split) | [`Split<Q::Dual, P::Dual>`](Split) |
//! | [`Loop<P>`](Loop) | Whatever operations are available for `P` | [`Loop<P::Dual>`](Loop) |
//! | [`Continue<N = Z>`](Continue) | Whatever operations are available for the start of the `N`th-innermost [`Loop`] | [`Continue<N>`](Continue) |
//! | [`Call<P, Q>`](Call) | Given a closure evaluating the session type `P` to `Done`, returns a result and a channel for the type `Q`:<br>[<code>let (t, c) = c.call(&#124;c&#124; async move { ... }).await?;</code>](Chan::call) | [`Call<P::Dual, Q::Dual>`](Call) |
//! | [`Done`] | Closes the channel, dropping its receive/transmit ends: [`c.close();`](Chan::close) | [`Done`] | [`c.close()`](Chan::close) |

#![recursion_limit = "256"]
#![allow(clippy::type_complexity)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
#![forbid(broken_intra_doc_links)]
#![cfg_attr(docsrs, feature(doc_cfg))]
use std::marker;

#[macro_use]
extern crate derivative;

#[allow(unused_imports)]
use crate::backend::*;

#[allow(unused_imports)]
use crate::prelude::*;

use futures::Future;

/// The prelude module for quickly getting started with Dialectic.
///
/// This module is designed to be imported as `use dialectic::prelude::*;`, which brings into scope
/// all the bits and pieces you need to start writing programs with Dialectic.
pub mod prelude {
    #[doc(no_inline)]
    pub use crate::backend::{Choice, Receive, Transmit};
    #[doc(no_inline)]
    pub use crate::session::Session;
    pub use crate::tuple::{List, Tuple};
    #[doc(no_inline)]
    pub use crate::types::{Call, Choose, Continue, Done, Loop, Offer, Recv, Send, Split};
    pub use crate::unary::constants::*;
    pub use crate::unary::types::*;
    pub use crate::unary::{LessThan, Unary, S, Z};
    #[doc(no_inline)]
    pub use crate::{offer, Branches, Chan, IncompleteHalf, SessionIncomplete, Unavailable};
    #[doc(no_inline)]
    pub use call_by::{CallBy, CallingConvention, Mut, Ref, Val};
    pub use dialectic_macro::Session;
}

pub mod backend;
pub mod tuple;
pub mod tutorial;
pub mod types;
pub mod unary;

mod session;
pub use session::Session;

mod chan;
#[doc(inline)]
pub use chan::{Branches, Chan};

/// Offer a set of different protocols, allowing the other side of the channel to choose with which
/// one to proceed. This macro only works in a `Try` context, i.e. somewhere the `?` operator would
/// make sense to use.
///
/// # Notes
///
/// - You must specify exactly as many branches as there are options in the type of the
///   [`Offer`](crate::types::Offer) to which this expression corresponds, and they must be in the
///   same order as the choices are in the tuple [`Offer`](crate::types::Offer)ed.
/// - In the body of each branch, the identifier for the channel is rebound to have the session type
///   corresponding to that branch.
/// - To use `offer!` as an expression, ensure the type of every branch matches.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::backend::mpsc;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// type GiveOrTake = Choose<(Send<i64, Done>, Recv<String, Done>)>;
///
/// let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
///
/// // Spawn a thread to offer a choice
/// let t1 = tokio::spawn(async move {
///     offer!(c2 => {
///         _0 => { c2.recv().await?; },
///         _1 => { c2.send("Hello!".to_string()).await?; },
///     });
///     Ok::<_, mpsc::Error>(())
/// });
///
/// // Choose to send an integer
/// c1.choose(_0).await?.send(42).await?;
///
/// // Wait for the offering thread to finish
/// t1.await??;
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! offer {
    (
        $chan:ident => { $($t:tt)* }
    ) => (
        {
            match $crate::Chan::offer($chan).await {
                Ok(b) => $crate::offer!{@branches b, $chan, $crate::unary::Z, $($t)* },
                Err(e) => Err(e)?,
            }
        }
    );
    (
        @branches $branch:ident, $chan:ident, $n:ty, $(,)?
    ) => (
        $crate::Branches::empty_case($branch),
    );
    (
        @branches $branch:ident, $chan:ident, $n:ty, $label:expr => $code:expr $(,)?
    ) =>
    (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => {
                let _: $n = $label;
                $code
            },
            std::result::Result::Err($branch) => $crate::Branches::empty_case($branch),
        }
    );
    (
        @branches $branch:ident, $chan:ident, $n:ty, $label:expr => $code:expr, $($t:tt)+
    ) => (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => {
                let _: $n = $label;
                $code
            },
            std::result::Result::Err($branch) => {
                $crate::offer!{@branches $branch, $chan, $crate::unary::S<$n>, $($t)+ }
            },
        }
    );
}

/// A placeholder for a missing [`Transmit`] or [`Receive`] end of a connection.
///
/// When using [`split`](Chan::split), the resultant two channels can only send or only receive,
/// respectively. This is reflected at the type level by the presence of [`Unavailable`] on the type
/// of the connection which *is not* present for each part of the split.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Unavailable;

/// The error returned when a closure which is expected to complete a channel's session fails to
/// finish the session of the channel it is given.
///
/// This error can arise either if the channel is dropped *before* its session is completed, or if
/// it is stored somewhere and is dropped *after* the closure's future is finished. The best way to
/// ensure this error does not occur is to call [`close`](Chan::close) on the channel,
/// which statically ensures it is dropped exactly when the session is complete.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum SessionIncomplete<Tx, Rx> {
    /// Both the sending half `Tx` and the receiving half `Rx` did not complete the session
    /// correctly.
    BothHalves {
        /// The incomplete sending half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        tx: IncompleteHalf<Tx>,
        /// The incomplete receiving half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        rx: IncompleteHalf<Rx>,
    },
    /// Only the sending half `Tx` did not complete the session correctly, but the receiving half
    /// `Rx` did complete it correctly.
    TxHalf {
        /// The incomplete sending half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        tx: IncompleteHalf<Tx>,
        /// The receiving half, whose session was completed.
        #[derivative(Debug = "ignore")]
        rx: Rx,
    },
    /// Only the receiving half `Rx` did not complete the session correctly, but the sending half
    /// `Tx` did complete it correctly.
    RxHalf {
        /// The sending half, whose session was completed.
        #[derivative(Debug = "ignore")]
        tx: Tx,
        /// The incomplete receiving half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        rx: IncompleteHalf<Rx>,
    },
}

/// A representation of what has gone wrong when a connection half `Tx` or `Rx` is incomplete.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum IncompleteHalf<T> {
    /// The underlying channel was dropped before the session was `Done`.
    Unfinished(#[derivative(Debug = "ignore")] T),
    /// The underlying channel was not dropped or [`close`](Chan::close)d after the session
    /// was `Done`.
    Unclosed,
}

impl<T> std::error::Error for IncompleteHalf<T> {}

impl<T> std::fmt::Display for IncompleteHalf<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "incomplete session or sub-session: channel half ")?;
        write!(
            f,
            "{}",
            match self {
                IncompleteHalf::Unfinished(_) => "was dropped before the session was `Done`",
                IncompleteHalf::Unclosed => "was not closed after the session was `Done`",
            }
        )
    }
}

impl<Tx, Rx> SessionIncomplete<Tx, Rx> {
    /// Extract the send and receive halves `Tx` and `Rx`, if they are present, from this
    /// `SessionIncomplete` error.
    pub fn into_halves(
        self,
    ) -> (
        Result<Tx, IncompleteHalf<Tx>>,
        Result<Rx, IncompleteHalf<Rx>>,
    ) {
        match self {
            SessionIncomplete::BothHalves { tx, rx } => (Err(tx), Err(rx)),
            SessionIncomplete::TxHalf { tx, rx } => (Err(tx), Ok(rx)),
            SessionIncomplete::RxHalf { tx, rx } => (Ok(tx), Err(rx)),
        }
    }
}

impl<Tx, Rx> std::error::Error for SessionIncomplete<Tx, Rx> {}

impl<Tx, Rx> std::fmt::Display for SessionIncomplete<Tx, Rx> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use IncompleteHalf::*;
        write!(f, "incomplete session or sub-session: channel")?;
        let reason = match self {
            SessionIncomplete::BothHalves { tx, rx } => match (tx, rx) {
                (Unclosed, Unclosed) => " was not closed after the session was `Done`",
                (Unclosed, Unfinished(_)) => {
                    "'s sending half was not closed after the session was `Done` \
                    and its receiving half was dropped before the session was `Done`"
                }
                (Unfinished(_), Unclosed) => {
                    "'s sending half was dropped before the session was `Done` \
                    and its receiving half was not closed after the session was `Done`"
                }
                (Unfinished(_), Unfinished(_)) => " was dropped before the session was `Done`",
            },
            SessionIncomplete::TxHalf { tx, .. } => match tx {
                Unfinished(_) => "'s sending half was dropped before the session was `Done`",
                Unclosed => "'s sending half was not closed after the session was `Done`",
            },
            SessionIncomplete::RxHalf { rx, .. } => match rx {
                Unfinished(_) => "'s receiving half was dropped before the session was `Done`",
                Unclosed => "'s receiving half was not closed after the session was `Done`",
            },
        };
        write!(f, "{}", reason)
    }
}
