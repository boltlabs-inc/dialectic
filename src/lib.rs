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
//! - gracefully handles runtime protocol violations, introducing **no panics**; and
//! - allows for **full duplex concurrent communication**, if specified in its type, while
//!   preserving all the same session-type safety guarantees.
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
//! - If you want to **integrate your own channel type** with Dialectic, you need to implement the
//!   [`Transmit`] and [`Receive`] traits from the [`backend`] module.
//! - Or, you can **[dive into the reference documentation](#modules)**...
//!
//! # Quick reference
//!
//! The **[tutorial]** covers all the constructs necessary to write session-typed programs with
//! Dialectic. A quick summary:
//!
//! - To make a pair of dual [`Chan`]s for a session type `P`: [`let (c1, c2) = P::channel(||
//!   {...})`](NewSession::channel) with some closure that builds a unidirectional underlying
//!   channel.
//! - To wrap an existing sender `tx` and receiver `rx` in a single [`Chan`] for `P`: [`let c =
//!   P::wrap(tx, rx)`](NewSession::wrap).
//! - Backend transports suitable for being wrapped in a [`Chan`] are provided in [`backend`], along
//!   with the [`Transmit`] and [`Receive`] traits necessary to implement your own.
//!
//! Once you've got a channel, here's what you can do:
//!
//! | Session Type (`S`) | Channel Operation(s) (on a channel `c: Chan<_, _, S, _>`) | Dual Type (`S::Dual`) |
//! | :----------- | :------------------- | :-------- |
//! | [`Send<T, P = Done>`](Send) | Given some `t: T`, returns a new `c`:<br>[`let c = c.send(t).await?;`](CanonicalChan::send) | [`Recv<T, P::Dual>`](Recv) |
//! | [`Recv<T, P = Done>`](Recv) | Returns some `t: T` and a new `c`:<br>[`let (t, c) = c.recv().await?;`](CanonicalChan::recv) | [`Send<T, P::Dual>`](Send) |
//! | [`Choose<Choices>`](Choose) | Given some `_N` < the length of `Choices`, returns a new `c`:<br>[`let c = c.choose(_N).await?;`](CanonicalChan::choose) | [`Offer<Choices::Dual>`](Offer) |
//! | [`Offer<Choices>`](Offer) | Given a set of labeled branches `_N => ...` in ascending order, exactly one for each option in the tuple `Choices`, returns a new `c` whose type each branch must match:<br>[`let c = offer!(c => { _0 => ..., _1 => ..., ... });`](offer!) | [`Choose<Choices::Dual>`](Choose) |
//! | [`Split<P, Q>`](Split) | Returns a pair of a [`Send`]/[`Choose`]-only and a [`Recv`]/[`Offer`]-only `tx` and `rx`, respectively (can be used concurrently):<br>[`let (tx, rx) = c.split();`](CanonicalChan::split)<br>When their types match again, given the two ends, returns a unified `c` with all capabilities:<br>[`let c = tx.unsplit_with(rx)?;`](CanonicalChan::unsplit_with) | [`Split<Q::Dual, P::Dual>`](Split) |
//! | [`Loop<P>`](Loop) | Whatever operations are available for `P` | [`Loop<P::Dual>`](Loop) |
//! | [`Continue<N = Z>`](Continue) | Whatever operations are available for the start of the `N`th-innermost [`Loop`] | [`Continue<N>`](Continue) |
//! | [`Break<N = Z>`](Break) | • If exiting the *outermost* [`Loop`]: Returns the underlying [`Transmit`]/[`Receive`] ends: [`let (tx, rx) = c.close();`](CanonicalChan::close)<br> • If exiting an *inner* [`Loop`]: Whatever operations are available for the start of the `(N + 1)`th-innermost [`Loop`] | [`Break<N>`](Break) |
//! | [`Seq<P, Q>`](Seq) | Given a closure evaluating the session type `P` to `Done`, returns a result and a channel for the type `Q`:<br>[<code>let (t, c) = c.seq(&#124;c&#124; async move { ... }).await?;</code>](CanonicalChan::seq) | [`Seq<P::Dual, Q::Dual>`](Seq) |
//! | [`Done`] | • If *outside* a [`Loop`]: Returns the underlying [`Transmit`]/[`Receive`] ends: [`let (tx, rx) = c.close();`](CanonicalChan::close)<br> • If *inside* a [`Loop`], equivalent to [`Continue`]: whatever operations are available for the start of the innermost [`Loop`] | [`Done`] | [`c.close()`](CanonicalChan::close) |

#![recursion_limit = "256"]
#![allow(clippy::type_complexity)]
#![warn(missing_docs, missing_doc_code_examples)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
#![forbid(broken_intra_doc_links)]
#![cfg_attr(docsrs, feature(doc_cfg))]
use std::{
    marker::{self, PhantomData},
    pin::Pin,
};

use crate::backend::*;
use futures::Future;

/// The prelude module for quickly getting started with Dialectic.
///
/// This module is designed to be imported as `use dialectic::prelude::*;`, which brings into scope
/// all the bits and pieces you need to start writing programs with Dialectic.
pub mod prelude {
    use super::*;
    pub use super::{canonical::CanonicalChan, offer, Branches, Chan, SeqError, UnsplitError};
    pub use backend::{Choice, Receive, Transmit};
    pub use call_by::{CallBy, CallingConvention, Mut, Ref, Val};
    pub use new_session::NewSession;
    pub use tuple::{List, Tuple};
    pub use types::unary::constants::*;
    pub use types::unary::types::*;
    pub use types::unary::{LessThan, Unary, S, Z};
    pub use types::*;
}

pub mod backend;
pub mod tutorial;
pub mod types;

mod new_session;
pub use new_session::NewSession;

pub mod canonical;
use canonical::CanonicalChan;

use prelude::*;

/// A bidirectional communications channel using the session type `P` over the connections `Tx` and
/// `Rx`.
///
/// ⚠️ **Important: always write this type synonym ([`Chan`]) in type signatures, *not️
/// [`CanonicalChan`] directly*.** This is because the [`Chan`] type synonym canonicalizes its
/// session type argument, which means it can be used more flexibly.
///
/// **[See the documentation for `CanonicalChan` for available methods and trait
/// implementations.](CanonicalChan)**
pub type Chan<Tx, Rx, P, E = ()> =
    CanonicalChan<Tx, Rx, <P as Actionable<E>>::Action, <P as Actionable<E>>::Env>;

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
/// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
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
            match $crate::canonical::CanonicalChan::offer($chan).await {
                Ok(b) => $crate::offer!{@branches b, $chan, $crate::types::unary::Z, $($t)* },
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
                $crate::offer!{@branches $branch, $chan, $crate::types::unary::S<$n>, $($t)+ }
            },
        }
    );
}

/// The result of [`offer`](CanonicalChan::offer): an enumeration of the possible new channel states
/// that could result from the offering of the tuple of protocols `Choices`.
///
/// To find out which protocol was selected by the other party, use [`Branches::case`], or better
/// yet, use the [`offer!`](crate::offer) macro to ensure you don't miss any cases.
///
/// **When possible, prefer the [`offer!`] macro over using [`Branches`] and
/// [`case`](Branches::case).**
#[derive(Debug)]
#[must_use]
pub struct Branches<Tx, Rx, Choices, E = ()>
where
    Choices: Tuple,
    Choices::AsList: EachActionable<E>,
    E: Environment,
{
    variant: u8,
    tx: Tx,
    rx: Rx,
    protocols: PhantomData<Choices>,
    environment: PhantomData<E>,
}

impl<'a, Tx, Rx, Choices, P, Ps, E> Branches<Tx, Rx, Choices, E>
where
    Choices: Tuple<AsList = (P, Ps)>,
    (P, Ps): List<AsTuple = Choices>,
    P: Actionable<E>,
    Ps: EachActionable<E> + List,
    E: Environment,
{
    /// Check if the selected protocol in this [`Branches`] was `P`. If so, return the corresponding
    /// channel; otherwise, return all the other possibilities.
    #[must_use = "all possible choices must be handled (add cases to match the type of this `Offer<...>`)"]
    pub fn case(
        self,
    ) -> Result<
        Chan<Tx, Rx, <P as Actionable<E>>::Action, <P as Actionable<E>>::Env>,
        Branches<Tx, Rx, Ps::AsTuple, E>,
    > {
        let variant = self.variant;
        let tx = self.tx;
        let rx = self.rx;
        if variant == 0 {
            Ok(canonical::CanonicalChan::from_raw_unchecked(tx, rx))
        } else {
            Err(Branches {
                variant: variant - 1,
                tx,
                rx,
                protocols: PhantomData,
                environment: PhantomData,
            })
        }
    }
}

impl<'a, Tx, Rx, E: Environment> Branches<Tx, Rx, (), E> {
    /// Attempt to eliminate an empty [`Branches`], returning an error if the originating
    /// discriminant for this set of protocol choices was out of range.
    ///
    /// This function is only callable on empty [`Branches`], which under ordinary circumstances
    /// means it proves the unreachability of its calling location. However, if the other end of the
    /// channel breaks protocol, an empty [`Branches`] can in fact be constructed, and this function
    /// will then signal an error.
    pub fn empty_case<T>(self) -> T {
        unreachable!("empty `Branches` cannot be constructed")
    }
}

/// A placeholder for a missing [`Transmit`] or [`Receive`] end of a connection.
///
/// When using [`split`](CanonicalChan::split), the resultant two channels can only send or only
/// receive, respectively. This is reflected at the type level by the presence of [`Unavailable`] on
/// the type of the connection which *is not* present for each part of the split, and [`Available`]
/// on the type of the connection which *is*.
#[derive(Debug)]
pub struct Unavailable<T>(PhantomData<T>);

impl<T> Unavailable<T> {
    /// Make a new `Unavailable`.
    fn new() -> Self {
        Unavailable(PhantomData)
    }
}

/// An available [`Transmit`] or [`Receive`] end of a connection.
///
/// When using [`split`](CanonicalChan::split), the resultant two channels can only send or only
/// receive, respectively. This is reflected at the type level by the presence of [`Available`] on
/// the type of the connection which *is* present for each part of the split, and [`Unavailable`] on
/// the type of the connection which *is not*.
///
/// Whenever `C` implements [`Transmit`] or [`Receive`], so does `Available<C>`.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash, Default)]
pub struct Available<C>(C);

impl<C> Available<C> {
    /// Retrieve the inner `C` connection.
    pub fn into_inner(self) -> C {
        self.0
    }
}

impl<C> AsRef<C> for Available<C> {
    fn as_ref(&self) -> &C {
        &self.0
    }
}

impl<C> AsMut<C> for Available<C> {
    fn as_mut(&mut self) -> &mut C {
        &mut self.0
    }
}

impl<T, Convention: CallingConvention, C> Transmit<T, Convention> for Available<C>
where
    C: Transmit<T, Convention>,
{
    type Error = C::Error;

    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + marker::Send + 'async_lifetime>>
    where
        T: CallBy<'a, Convention>,
        <T as CallBy<'a, Convention>>::Type: marker::Send,
        'a: 'async_lifetime,
    {
        self.0.send(message)
    }
}

impl<T, C> Receive<T> for Available<C>
where
    C: Receive<T>,
{
    type Error = C::Error;

    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + marker::Send + 'async_lifetime>>
    {
        self.0.recv()
    }
}

/// The error returned when two split channel parts cannot be
/// [`unsplit_with`](CanonicalChan::unsplit_with) each other because they originated from
/// different channels.
#[derive(Debug)]
pub struct UnsplitError<Tx, Rx, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    /// The transmit-only half.
    tx: CanonicalChan<Available<Tx>, Unavailable<Rx>, P, E>,

    /// The receive-only half.
    rx: CanonicalChan<Unavailable<Tx>, Available<Rx>, P, E>,
}

impl<Tx, Rx, P, E> UnsplitError<Tx, Rx, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    /// Extract from this error the transmit-only and receive-only [`Chan`]s which could not be
    /// [`unsplit_with`](CanonicalChan::unsplit_with) each other.
    pub fn into_ends(
        self,
    ) -> (
        Chan<Available<Tx>, Unavailable<Rx>, P, E>,
        Chan<Unavailable<Tx>, Available<Rx>, P, E>,
    ) {
        (self.tx, self.rx)
    }
}

impl<Tx, Rx, P, E> std::error::Error for UnsplitError<Tx, Rx, P, E>
where
    Tx: std::fmt::Debug,
    Rx: std::fmt::Debug,
    P: Actionable<E, Action = P, Env = E> + std::fmt::Debug,
    E: Environment + std::fmt::Debug,
{
}

impl<Tx, Rx, P, E> std::fmt::Display for UnsplitError<Tx, Rx, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "cannot `Chan::unsplit_with` two unrelated channels")
    }
}

/// The error returned when the closure passed to [`seq`](CanonicalChan::seq) returns a different
/// [`Chan`] than the one it was passed as input. This is illegal and must result in an error,
/// because otherwise the session for the channel returned by [`seq`](CanonicalChan::seq) would be
/// potentially invalid, since we don't know the true session type of the channel.
#[derive(Debug, Copy, Clone)]
pub struct SeqError {
    _private: (),
}

impl std::error::Error for SeqError {}

impl std::fmt::Display for SeqError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "a closure passed to `Chan::seq` returned a different `Chan` than its input"
        )
    }
}
