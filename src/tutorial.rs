//! The introductory tutorial for Dialectic (nothing is exported from this module).
//!
//! # Getting started with Dialectic
//!
//! The first step to communicating is figuring out what you mean to say.
//!
//! This crate provides a concise [type-level language for expressing session types](crate::types).
//! The session type attached to a wrapped communications channel indicates precisely which actions
//! are available for each end of the channel.
//!
//! Let's write our first session type:
//!
//! ```
//! use dialectic::prelude::*;
//!
//! type JustSendOneString = Send<String, Done>;
//! ```
//!
//! This type specifies a very simple protocol: "send a string, then finish." The first argument to
//! `Send` is the type sent, and the second is the rest of the protocol, which in this case is the
//! empty protocol [`Done`].
//!
//! Every session type has a [`Dual`](crate::Session::Dual), which describes what the other end of
//! the channel must do to follow the channel's protocol; if one end [`Send`]s, the other end must
//! [`Recv`]:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(<Send<String, Done> as Session>::Dual, Recv<String, Done>);
//! ```
//!
//! (Here and elsewhere in this tutorial, we use the
//! [`assert_type_eq_all!`](static_assertions::assert_type_eq_all) macro from the
//! [`static_assertions`] crate to assert that Rust sees these types as equal.)
//!
//! Because many sessions end with either a `Send` or a `Recv`, their types can be abbreviated when
//! the rest of the session is `Done`:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(Send<String>, Send<String, Done>);
//! assert_type_eq_all!(Recv<String>, Recv<String, Done>);
//! ```
//!
//! Given a valid session type, we can wrap an underlying communications channel with it. Here,
//! let's make two ends of a channel for playing out our `JustSendOneString` protocol.
//!
//! In this case, we're using the [`mpsc`](crate::backend::mpsc) backend Dialectic provides, which
//! is built on [`tokio::sync::mpsc`]. However, the mechanism for wrapping underlying channels is
//! extensible, meaning you can choose your own transport if you want.
//!
//! ```
//! # use dialectic::prelude::*;
//! use dialectic::backend::mpsc;
//! ```
//!
//! The static method [`channel`](crate::NewSession::channel) is automatically defined for all valid
//! session types (note: its corresponding trait [`NewSession`] needs to be in scope for it to be
//! callable). It takes as input a closure that creates some underlying unidirectional transport
//! channel, and creates a matched pair of bidirectional session-typed channels wrapping the
//! underlying channel type.
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! ```
//!
//! The types of `c1` and `c2` are inferred from the session type specified: `c1`'s type corresponds
//! to the given session type, and `c2`'s type corresponds to its dual:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! let _: Chan<mpsc::Sender, mpsc::Receiver, Send<String>> = c1;
//! let _: Chan<mpsc::Sender, mpsc::Receiver, Recv<String>> = c2;
//! ```
//!
//! Now that we have the two ends of a bidirectional session-typed channel, we can use them to
//! concurrently enact the protocol specified by their type. In this case, we're going to run them
//! in two parallel [`tokio`] tasks. However, Dialectic is generic over the underlying async
//! runtime, provided the underlying transport channel is of a compatible type.
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! tokio::spawn(async move {
//!     c1.send("Hello, world!".to_string()).await?;
//!     Ok::<_, mpsc::Error>(())
//! });
//!
//! let (greeting, c2) = c2.recv().await?;
//! assert_eq!(greeting, "Hello, world!");
//! # Ok(())
//! # }
//! ```
//!
//! 🎉 **Tada!** We've just written an asynchronous session-typed program with Dialectic!
//!
//! # Moving forward
//!
//! Almost every operation on a [`Chan`]:
//!
//! - is **asynchronous** due to the inherent asynchrony of the protocol,
//! - is **fallible** to account for issues in the underlying transport channel,
//! - and **takes ownership** of the `Chan` upon which it is invoked to enforce type correctness.
//!
//! As a result, each invocation of an operation on a channel is usually followed by an `.await?`,
//! and the result of each operation should usually be rebound via `let` to the same name as the
//! original channel.
//!
//! In this example, two interlocking threads collaborate to compute the parity of the length of the
//! string "Hello, world!". Notice how each time a channel operation is invoked, its name is rebound
//! to a new channel.
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! type ParityOfLength = Send<String, Recv<usize, Send<bool>>>;
//! let (c1, c2) = ParityOfLength::channel(|| mpsc::channel(1));
//!
//! // The string whose length's parity we'll compute
//! let string = "Hello, world!".to_string();
//!
//! // Send "Hello, world!", receive its length, then send its parity
//! let t1 = tokio::spawn(async move {
//!     // `send` returns the channel
//!     let c1 = c1.send(string).await?;
//!
//!     // `recv` returns a pair of (received value, channel)
//!     let (len, c1) = c1.recv().await?;
//!
//!     // `send` returns the channel
//!     let c1 = c1.send(len % 2 == 0).await?;
//!
//!     Ok::<_, mpsc::Error>(())
//! });
//!
//! // Receive a string, send its length, and receive its parity
//! let t2 = tokio::spawn(async move {
//!     // `recv` returns a pair of (received value, channel)
//!     let (string, c2) = c2.recv().await?;
//!
//!     // `send` returns the channel
//!     let c2 = c2.send(string.chars().count()).await?;
//!
//!     // `recv` returns a pair of (received value, channel)
//!     let (parity, c2) = c2.recv().await?;
//!
//!     Ok::<_, mpsc::Error>(parity)
//! });
//!
//! // Wait for the tasks to finish
//! t1.await??;
//! let parity = t2.await??;
//!
//! // Check that the parity was correct
//! assert_eq!(parity, false);
//! # Ok(())
//! # }
//! ```
//!
//! # Getting stopped
//!
//! The most important benefit to session typing is not what it lets you do, but rather, what it
//! *prevents you from doing*. Below are some things we *can't do* with our `JustSendOneString`
//! channel from above:
//!
//! ```
//! # use dialectic::prelude::*;
//! type JustSendOneString = Send<String, Done>;
//! ```
//!
//! Trying to do any of the below things results in a compile-time type error (edited for brevity).
//!
//! If we try to send on the end of the channel that's meant to receive...
//!
//! ```compile_fail
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! c2.send("Hello, world!".to_string()).await?;
//! # Ok(())
//! # }
//! ```
//!
//! ...we get an error telling us exactly that:
//!
//! ```text
//! error[E0599]: no method named `send` found for struct `Chan<Sender, Receiver, Recv<String>>` in the current scope
//! ```
//!
//! If we try to receive the wrong type of thing...
//!
//! ```compile_fail
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! c1.send("Hello, world!".to_string()).await?;
//! let (n, c2): (i64, _) = c2.recv().await?;
//! # Ok(())
//! # }
//! ```
//!
//! ...we get an error informing us so:
//!
//! ```text
//! error[E0308]: try expression alternatives have incompatible types
//!    |
//!    | let (n, c2): (i64, _) = c2.recv().await?;
//!    |                         ^^^^^^^^^^^^^^^^ expected `i64`, found struct `String`
//! ```
//!
//! But the unique power session types bring to the table is enforcing the validity of *sequences*
//! of messages. If we try to send twice in a row...
//!
//! ```compile_fail
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! let c1 = c1.send("Hello, world!".to_string()).await?;
//! c1.send("Hello again!".to_string()).await?;
//! # Ok(())
//! # }
//! ```
//!
//! ...we get an error we saying the returned channel is now of type `Chan<_, _, Done>`, and we
//! can't send when it's the end:
//!
//! ```text
//! error[E0599]: no method named `send` found for struct `Chan<Sender, Receiver, Done>` in the current scope
//! ```
//!
//! # Branching out
//!
//! Most interesting protocols don't just consist of linear sequences of [`Send`]s and [`Recv`]s.
//! Sometimes, one party offers the other a choice of different ways to proceed, and the other
//! chooses which path to take.
//!
//! In Dialectic, this possibility is represented by the [`Offer`] and [`Choose`] types. Each is
//! parameterized by a *tuple* of session types representing, respectively, the choices offered by
//! one end of the channel, or the choices available to choose from.
//!
//! `Offer` is dual to `Choose` if the choices offered are dual to the choices available to choose
//! from:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(
//!     <Offer<(Send<String>, Recv<i64>)> as Session>::Dual,
//!     Choose<(Recv<String>, Send<i64>)>,
//! );
//! ```
//!
//! Just as the [`send`](CanonicalChan::send) and [`recv`](CanonicalChan::recv) methods enact the
//! [`Send`] and [`Recv`] session types, the [`choose`](CanonicalChan::choose) method and
//! [`offer!`](crate::offer) macro enact the [`Choose`] and [`Offer`] session types.
//!
//! Suppose we want to offer a choice between two protocols: either sending a single integer
//! (`Send<i64>`) or receiving a string (`Recv<String>`). Correspondingly, the other end of the
//! channel must indicate a choice of which protocol to follow, and we need to handle the result of
//! either selection by enacting the protocol chosen.
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!
//! let (c1, c2) = <Offer<(Send<i64>, Recv<String>)>>::channel(|| mpsc::channel(1));
//!
//! // Offer a choice
//! let t1 = tokio::spawn(async move {
//!     let c1 = offer!(c1 => {
//!         _0 => c1.send(42).await?,  // handle `c2.choose(_0)`
//!         _1 => c1.recv().await?.1,  // handle `c2.choose(_1)`
//!     });
//! #   c1.close();
//!     Ok::<_, mpsc::Error>(())
//! });
//!
//! // Make a choice
//! let t2 = tokio::spawn(async move {
//!     let c2 = c2.choose(_1).await?;            // select to `Send<String>`
//!     c2.send("Hi there!".to_string()).await?;  // enact the selected choice
//!     Ok::<_, mpsc::Error>(())
//! });
//!
//! // Wait for the tasks to finish
//! t1.await??;
//! t2.await??;
//! # Ok(())
//! # }
//! ```
//!
//! In the above, we can see how the [`offer!`](crate::offer) macro takes the name of a channel
//! (this must be an identifier, not an expression) and a list of branches labeled by index which
//! may use that channel. In each expression, the channel's type corresponds to the session type for
//! that choice. The type of each expression in the list must be the same, which means that if we
//! want to bind a channel name to the result of the `offer!`, each expression must step the channel
//! forward to an identical session type (in the case above, that's `Done`).
//!
//! Dually, to select an offered option, you can call the [`choose`](CanonicalChan::choose) method
//! on a channel, passing it as input a constant corresponding to the index of the choice. These
//! constants are *not* Rust's built-in numeric types, but rather [unary type-level
//! numbers](crate::types::unary). Dialectic supports up to 128 possible choices in an `Offer` or
//! `Choose`, and the corresponding constants [`_0`](crate::types::unary::constants::_0),
//! [`_1`](crate::types::unary::constants::_1), [`_2`](crate::types::unary::constants::_2), ...
//! [`_127`](crate::types::unary::constants::_127) are defined in the
//! [`constants`](crate::types::unary::constants) module.
//!
//! # Looping back
//!
//! While some protocols can be described as a finite sequence of operations, many contain the
//! possibility of loops. Consider things like retrying after an invalid response, sending an
//! unbounded stream of messages, or providing a persistent connection for multiple queries. All
//! these can be modeled with session types by introducing *recursion*.
//!
//! Suppose we want to send a stream of as many integers as desired, then receive back their sum. We
//! could describe this protocol using the [`Loop`], [`Continue`], and [`Break`] types:
//!
//! ```
//! # use dialectic::prelude::*;
//! type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64, Break>)>>;
//! ```
//!
//! **Notice:** We have to *explicitly* use [`Continue`] to reiterate the [`Loop`]: even inside a
//! [`Loop`], [`Done`] means that the session is over.
//!
//! The dual to `Loop<P>` is `Loop<P::Dual>`, the dual to `Continue` is `Continue`, and the dual to
//! `Break` is `Break`, so we know the other end of this channel will need to implement:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use static_assertions::assert_type_eq_all;
//! # type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64, Break>)>>;
//! type ComputeSum = Loop<Offer<(Recv<i64, Continue>, Send<i64, Break>)>>;
//! assert_type_eq_all!(<QuerySum as Session>::Dual, ComputeSum);
//! ```
//!
//! We can implement this protocol by following the session types. When the session type of a
//! [`Chan`] hits a [`Continue`] point, it jumps back to the type of the [`Loop`] to which that
//! [`Continue`] refers. In this case, for example, immediately after the querying task sends an
//! integer, the resultant channel will have the session type `Choose<(Send<i64, Continue>,
//! Recv<i64>)>` once more.
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64, Break>)>>;
//! # type ComputeSum = Loop<Offer<(Recv<i64, Continue>, Send<i64, Break>)>>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! #
//! let (mut c1, mut c2) = QuerySum::channel(|| mpsc::channel(1));
//!
//! // Sum all the numbers sent over the channel
//! tokio::spawn(async move {
//!     let mut sum = 0;
//!     let c2 = loop {
//!         c2 = offer!(c2 => {
//!             _0 => {
//!                 let (n, c2) = c2.recv().await?;
//!                 sum += n;
//!                 c2
//!             },
//!             _1 => break c2,
//!         });
//!     };
//!     c2.send(sum).await?.close();
//!     Ok::<_, mpsc::Error>(())
//! });
//!
//! // Send some numbers to be summed
//! for n in 0..=10 {
//!     c1 = c1.choose(_0).await?.send(n).await?;
//! }
//!
//! // Get the sum
//! let (sum, c1) = c1.choose(_1).await?.recv().await?;
//! c1.close();
//! assert_eq!(sum, 55);
//! # Ok(())
//! # }
//! ```
//!
//! **Notice:** Whenever we loop over a channel, the channel itself needs to be declared `mut`. This
//! is because each channel operation takes the channel as an owned value, returning a new channel.
//! In order to repeat a loop, we need to re-assign to the channel at the end of the loop so that it
//! is in scope for the next iteration.
//!
//! ## Nested loops
//!
//! If the protocol contains nested loops, you can specify which nested loop to continue with  using
//! the optional parameter of `Continue`. By default, `Continue` jumps to the innermost loop;
//! however, `Continue<_1>` jumps to the second-innermost, `Continue<_2>` the third-innermost, etc.
//! Likewise, `Break` breaks out of the innermost loop, `Break<_1>` breaks out of the
//! second-innermost, `Break<_2>` the third-innermost, etc. The types [`_0`](unary::types::_0),
//! [`_1`](unary::types::_1), [`_2`](unary::types::_2), etc. are defined in the [`unary::types`]
//! module and imported by default in the [`dialectic::*`](crate) namespace.
//!
//! ## Automatic looping
//!
//! You may have noticed how in the example above, [`choose`](CanonicalChan::choose) can be called
//! on `c1` even though the outermost part of `c1`'s session type `QuerySum` would seem not to begin
//! with [`Choose`]. This is true in general: if the session type of a [`Chan`] is a [`Loop`],
//! [`Break`], or [`Continue`] that leads to a session type for which a given operation is valid,
//! that operation is valid on the [`Chan`]. In the instance above, calling
//! [`choose`](CanonicalChan::choose) on a [`Chan`] with session type `Loop<Choose<...>>` works, no
//! matter how many `Loop`s enclose the `Choose`. Similarly, if a `Chan`'s type is `Continue`,
//! whatever operation would be valid for the session type at the start of the corresponding `Loop`
//! is valid for that `Chan`.
//!
//! This behavior is enabled by the [`Actionable`] trait, which defines what the next "real action"
//! on a session type is. For most session types, the "real action" is that session type itself.
//! However, for only [`Loop`], [`Break`], and [`Continue`], the next action is whatever follows
//! entering the loop(s) or recurring, respectively.
//!
//! In most uses of Dialectic, you won't need to directly care about the [`Actionable`] trait or
//! most of the traits in [`types`](crate::types) aside from [`Session`]. It's good to know what
//! it's for, though, because that might help you understand an error message more thoroughly in the
//! future!
//!
//! # Splitting off
//!
//! Traditional presentations of session types do not allow the channel to be used concurrently to
//! send and receive at the same time. Some protocols, however, can be made more efficient by
//! executing certain portions of them in parallel.
//!
//! Dialectic incorporates this option into its type system with the [`Split`] type. A channel with
//! a session type of `Split<P, Q>` can be [`split`](CanonicalChan::split) into a send-only end with
//! the session type `P` and a receive-only end with the session type `Q`, which can then be used
//! concurrently. In the example below, the two ends of a channel **concurrently swap** a
//! `Vec<usize>` and a `String`. If this example were run over a network and these values were
//! large, this could represent a significant reduction in runtime.
//!
//! ```
//! # use dialectic::prelude::*;
//! type SwapVecString = Split<Send<Vec<usize>>, Recv<String>>;
//! ```
//!
//! The dual of `Split<P, Q>` is `Split<Q::Dual, P::Dual>`:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(
//!     <Split<Send<Vec<usize>>, Recv<String>> as Session>::Dual,
//!     Split<Send<String>, Recv<Vec<usize>>>,
//! );
//! ```
//!
//! Notice that `P` and `Q` switch places! This is because the left-hand `P` is always the send-only
//! session, and the right-hand `Q` is always the receive-only session.
//!
//! Now, let's use a channel of this session type to enact a concurrent swap of a `String` and a
//! `Vec<usize>`:
//!
//! ```
//! use dialectic::prelude::*;
//! use dialectic::backend::mpsc;
//!
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! type SendAndRecv = Split<Send<Vec<usize>>, Recv<String>>;
//!
//! let (c1, c2) = SendAndRecv::channel(|| mpsc::channel(1));
//!
//! // Spawn a thread to simultaneously send a `Vec<usize>` and receive a `String`:
//! let t1 = tokio::spawn(async move {
//!     c1.split(|tx, rx| async move {
//!         // In one thread we send the `Vec`...
//!         let send_vec = tokio::spawn(async move {
//!             tx.send(vec![1, 2, 3, 4, 5]).await?;
//!             Ok::<_, mpsc::Error>(())
//!         });
//!         // In another thread we receive the `String`...
//!         let recv_string = tokio::spawn(async move {
//!             let (string, _) = rx.recv().await?;
//!             Ok::<_, mpsc::Error>(string)
//!         });
//!         send_vec.await.unwrap()?;
//!         let string = recv_string.await.unwrap()?;
//!         Ok::<_, mpsc::Error>(string)
//!     }).await
//! });
//!
//! // Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
//! // the task above:
//! c2.split(|tx, rx| async move {
//!     // In one thread we send the `String`...
//!     let send_string = tokio::spawn(async move {
//!         tx.send("Hello!".to_string()).await?;
//!         Ok::<_, mpsc::Error>(())
//!     });
//!     // In another thread we receive the `Vec`...
//!     let recv_vec = tokio::spawn(async move {
//!         let (vec, _) = rx.recv().await?;
//!         Ok::<_, mpsc::Error>(vec)
//!     });
//!
//!     // Examine the result values:
//!     send_string.await??;
//!     let vec = recv_vec.await??;
//!     let string = t1.await??.0;
//!     assert_eq!(vec, &[1, 2, 3, 4, 5]);
//!     assert_eq!(string, "Hello!");
//!
//!     Ok::<_, Box<dyn std::error::Error>>(())
//! }).await?;
//! #
//! # Ok(())
//! # }
//! ```
//!
//! When using [`Split`], keep in mind its limitations:
//!
//! - It's a type error to [`Send`] or [`Choose`] on the receive-only end.
//! - It's a type error to [`Recv`] or [`Offer`] on the transmit-only end.
//! - It's a runtime [`SessionIncomplete`] error if you don't drop both the `tx` and `rx` ends
//!   before the future completes. This is subject to the same behavior as in
//!   [`seq`](CanonicalChan::seq), described below. [See here for more
//!   explanation](#errors-in-subroutines-what-not-to-do).
//!
//! # Sequencing and Modularity
//!
//! The final session type provided by Dialectic is the [`Seq`] type, which permits a more modular
//! way of constructing session types and their implementations. At first, you will likely not need
//! to use [`Seq`]; however, as you build larger programs, it makes it a lot easier to split them up
//! into smaller, independent specifications and components.
//!
//! So, what does it do? A session type `Seq<P, Q>` means "run the session `P`, then run the session
//! `Q`". You can think of it like the `;` in Rust, which concatenates separate statements. In
//! smaller protocols, you don't need to use [`Seq`] to sequence operations, because all the session
//! type primitives (with the exception of [`Split`]) take arguments indicating "what to do next":
//! we don't need to write `Seq<Send<String>, Recv<String>>`, because `Send<String, Recv<String>>`
//! works just as well.
//!
//! [`Seq`] becomes useful however when you already have a subroutine that implements *part of* a
//! session, and you'd like to use it in a larger context. Imagine, for example, that you had
//! already written the following:
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! type Query = Send<String, Recv<String, Done>>;
//!
//! async fn query<E: Environment>(
//!     question: String,
//!     chan: mpsc::Chan<Query, E>
//! ) -> Result<String, mpsc::Error> {
//!     let chan = chan.send(question).await?;
//!     let (answer, chan) = chan.recv().await?;
//!     chan.close();
//!     Ok(answer)
//! }
//! ```
//!
//! Then, at some later point in time, you might realize you need to implement a protocol that makes
//! several calls to `Query` in a loop. Unfortunately, the type of `Query` ends with [`Done`], and
//! its implementation `query` (correctly) closes the [`Chan`] at the end. Without
//! [`seq`](CanonicalChan::seq), you would have to modify both the type `Query` and the function
//! `query`, just to let them be called from a larger context.
//!
//! Instead of re-writing both the specification and the implementation, we can use [`Seq`] to
//! integrate `query` as a subroutine in a larger protocol, without changing its type or definition.
//! All we need to do is use the [`seq`](CanonicalChan::seq) method to call it as a subroutine on
//! the channel.
//!
//! ```
//! # use dialectic::prelude::*;
//! # use dialectic::backend::mpsc;
//! # type Query = Send<String, Recv<String, Done>>;
//! #
//! # async fn query<E: Environment>(
//! #     question: String,
//! #     chan: mpsc::Chan<Query, E>
//! # ) -> Result<String, mpsc::Error> {
//! #     let chan = chan.send(question).await?;
//! #     let (answer, chan) = chan.recv().await?;
//! #     chan.close();
//! #     Ok(answer)
//! # }
//! #
//! type MultiQuery = Loop<Choose<(Break, Seq<Query, Continue>)>>;
//!
//! async fn query_all(
//!     mut questions: Vec<String>,
//!     mut chan: mpsc::Chan<MultiQuery>
//! ) -> Result<Vec<String>, mpsc::Error> {
//!   // Result<Vec<String, mpsc::Error> {
//!     let mut answers = Vec::with_capacity(questions.len());
//!     for question in questions.into_iter() {
//!         let (answer, c) =
//!             chan.choose(_1).await?
//!                 .seq(|c| query(question, c)).await?;  // Call `query` as a subroutine
//!         chan = c.unwrap();
//!         answers.push(answer);
//!     }
//!     chan.choose(_0).await?.close();
//!     Ok(answers)
//! }
//! ```
//!
//! Furthermore, [`seq`](CanonicalChan::seq) can be used to implement **context free session
//! types**, where the sub-protocol in the first half `P` of `Seq<P, Q>` uses [`Continue`] to recur
//! back outside the [`Seq`] itself. This allows you to define recursive protocols that can be
//! shaped like any arbitrary tree. For more details and an example, see the documentation for
//! [`seq`](CanonicalChan::seq).
//!
//! ## Errors in subroutines (what not to do)
//!
//! In order for the [`seq`](CanonicalChan::seq) method to preserve the validity of the session type
//! `Seq<P, Q>`, we need to make sure that the subroutine executing `P` **finishes** the session `P`
//! by driving the channel to the `Done` session type. If we didn't check this, it would be possible
//! to drop the channel early in the subroutine, thus allowing steps to be skipped in the protocol.
//! The [`seq`](CanonicalChan::seq) method solves this problem by returning a pair of the
//! subroutines's return value and a [`Result`] which is a [`Chan`] for `Q` if `P` was completed
//! successfully, or a [`SessionIncomplete`] error if not. It's almost always a programmer error if
//! you get a [`SessionIncomplete`] error, so it's usually the right idea to
//! [`unwrap`](Result::unwrap) it and proceed without further fanfare.
//!
//! 💡 **A useful pattern:** If you make sure to *always* call [`close`](CanonicalChan::close) on
//! the channel before the subroutine's future returns, you can be guaranteed that such errors are
//! impossible, because [`close`](CanonicalChan::close) can only be called when a channel's session
//! is complete.
//!
//! # Wrapping up
//!
//! We've now finished our tour of everything you need to get started programming with Dialectic! ✨
//!
//! You might now want to...
//!
//! - Check out the documentation for **[`Chan`]**, if you haven't already?
//! - Learn how to instantiate (or implement) a **[`backend`](crate::backend)** other than the
//!   **[`mpsc`](crate::backend::mpsc)** backend we used in the tutorial above?
//! - Jump back to the top of **[reference documentation](crate#quick-reference)**?
//!
//! Thanks for following along, and enjoy!

// Import the whole crate so the docs above can link appropriately.
#![allow(unused_imports)]
use crate::prelude::*;
use static_assertions;
