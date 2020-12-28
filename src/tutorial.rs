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
//! use dialectic::*;
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
//! # use dialectic::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(<Send<String, Done> as Session>::Dual, Recv<String, Done>);
//! ```
//!
//! Because many sessions end with either a `Send` or a `Recv`, their types can be abbreviated when
//! the rest of the session is `Done`:
//!
//! ```
//! # use dialectic::*;
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
//! # use dialectic::*;
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
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! ```
//!
//! The types of `c1` and `c2` are inferred from the session type specified: `c1`'s type corresponds
//! to the given session type, and `c2`'s type corresponds to its dual:
//!
//! ```
//! # use dialectic::*;
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
//! # use dialectic::*;
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
//! # use dialectic::*;
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
//! # use dialectic::*;
//! type JustSendOneString = Send<String, Done>;
//! ```
//!
//! Trying to do any of the below things results in a compile-time type error (edited for brevity).
//!
//! If we try to send on the end of the channel that's meant to receive...
//!
//! ```compile_fail
//! # use dialectic::*;
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
//! # use dialectic::*;
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
//! # use dialectic::*;
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
//! # use dialectic::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(
//!     <Offer<(Send<String>, Recv<i64>)> as Session>::Dual,
//!     Choose<(Recv<String>, Send<i64>)>,
//! );
//! ```
//!
//! Just as the [`send`](Chan::send) and [`recv`](Chan::recv) methods enact the [`Send`] and
//! [`Recv`] session types, the [`choose`](Chan::choose) method and [`offer!`](crate::offer) macro
//! enact the [`Choose`] and [`Offer`] session types.
//!
//! Suppose we want to offer a choice between two protocols: either sending a single integer
//! (`Send<i64>`) or receiving a string (`Recv<String>`). Correspondingly, the other end of the
//! channel must indicate a choice of which protocol to follow, and we need to handle the result of
//! either selection by enacting the protocol chosen.
//!
//! ```
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, Done>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use dialectic::constants::*;
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
//! Dually, to select an offered option, you can call the [`choose`](Chan::choose) method on a
//! channel, passing it as input a constant corresponding to the index of the choice. These
//! constants are *not* Rust's built-in numeric types, but rather [unary type-level
//! numbers](crate::types::unary). Dialectic supports up to 128 possible choices in an `Offer` or
//! `Choose`, and the corresponding constants [`_0`](crate::constants::_0),
//! [`_1`](crate::constants::_1), [`_2`](crate::constants::_2), ... [`_127`](crate::constants::_127)
//! are defined in the [`constants`](crate::constants) module.
//!
//! # Looping back
//!
//! While some protocols can be described as a finite sequence of operations, many contain the
//! possibility of loops. Consider things like retrying after an invalid response, sending an
//! unbounded stream of messages, or providing a persistent connection for multiple queries. All
//! these can be modeled with session types by introducing *recursion*.
//!
//! Suppose we want to send a stream of as many integers as desired, then receive back their sum. We
//! could describe this protocol using the [`Loop`] and [`Continue`] types:
//!
//! ```
//! # use dialectic::*;
//! type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64>)>>;
//! ```
//!
//! The dual to `Loop<P>` is `Loop<P::Dual>`, and the dual to `Continue` is `Continue`, so we know
//! the other end of this channel will need to implement:
//!
//! ```
//! # use dialectic::*;
//! # use static_assertions::assert_type_eq_all;
//! # type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64>)>>;
//! type ComputeSum = Loop<Offer<(Recv<i64, Continue>, Send<i64>)>>;
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
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64>)>>;
//! # type ComputeSum = Loop<Offer<(Recv<i64, Continue>, Send<i64>)>>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # use dialectic::constants::*;
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
//!     c2.send(sum).await?;
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
//! The types [`_0`](unary::types::_0), [`_1`](unary::types::_1), [`_2`](unary::types::_2), etc. are
//! defined in the [`unary::types`] module.
//!
//! ## Automatic looping
//!
//! You may have noticed how in the example above, [`choose`](Chan::choose) can be called on `c1`
//! even though the outermost part of `c1`'s session type `QuerySum` would seem not to begin with
//! [`Choose`]. This is true in general: if the session type of a [`Chan`] either [`Loop`]s or
//! [`Continue`]s to a session type for which a given operation is valid, that operation is valid on
//! the [`Chan`]. In the instance above, calling [`choose`](Chan::choose) on a [`Chan`] with session
//! type `Loop<Choose<...>>` works, no matter how many `Loop`s enclose the `Choose`. Similarly, if a
//! `Chan`'s type is `Continue`, whatever operation would be valid for the session type at the start
//! of the corresponding `Loop` is valid for that `Chan`.
//!
//! This behavior is enabled by the [`Actionable`] trait, which defines what the next "real action"
//! on a session type is. For [`Done`], [`Send`], [`Recv`], [`Offer`], [`Choose`], and [`Split`]
//! (the final session type discussed below), the "real action" is that session type itself.
//! However, for [`Loop`] and [`Continue`], the next action is whatever follows entering the loop(s)
//! or recurring, respectively.
//!
//! In most uses of Dialectic, you won't need to directly care about the [`Actionable`] trait or
//! most of the traits in [`types`] aside from [`Session`]. It's good to know what it's for, though,
//! because that might help you understand an error message more thoroughly in the future!
//!
//! # Splitting off
//!
//! Traditional presentations of session types do not allow the channel to be used concurrently to
//! send and receive at the same time. Some protocols, however, can be made more efficient by
//! executing certain portions of them in parallel.
//!
//! Dialectic incorporates this option into its type system with the [`Split`] type. A channel with
//! a session type of `Split<P, Q>` can be [`split`](Chan::split) into a send-only end with the
//! session type `P` and a receive-only end with the session type `Q`, which can then be used
//! concurrently. In the example below, the two ends of a channel **concurrently swap** a
//! `Vec<usize>` and a `String`. If this example were run over a network and these values were
//! large, this could represent a significant reduction in runtime.
//!
//! ```
//! # use dialectic::*;
//! type SwapVecString = Split<Send<Vec<usize>>, Recv<String>>;
//! ```
//!
//! The dual of `Split<P, Q>` is `Split<Q::Dual, P::Dual>`:
//!
//! ```
//! # use dialectic::*;
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
//! Now, let's use a channel of this session type to enact a concurrent swap:
//!
//! ```
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # use dialectic::constants::*;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # type SwapVecString = Split<Send<Vec<usize>>, Recv<String>>;
//! let (c1, c2) = SwapVecString::channel(|| mpsc::channel(1));
//!
//! // Spawn a thread to simultaneously send a `Vec<usize>` and receive a `String`:
//! let t1 = tokio::spawn(async move {
//!     // Split c1 into a send-only `tx` and receive-only `rx`
//!     let (tx, rx) = c1.split();
//!
//!     // Concurrently send and receive
//!     let send_vec = tokio::spawn(async move {
//!         let tx = tx.send(vec![1, 2, 3, 4, 5]).await?;
//!         Ok::<_, mpsc::Error>(tx)
//!     });
//!     let recv_string = tokio::spawn(async move {
//!         let (string, rx) = rx.recv().await?;
//!         Ok::<_, mpsc::Error>((string, rx))
//!     });
//!     let tx = send_vec.await.unwrap()?;
//!     let (string, rx) = recv_string.await.unwrap()?;
//!
//!     // Unsplit the ends of `c1`
//!     let c1 = Chan::unsplit(tx, rx).unwrap();
//!
//!     Ok::<_, mpsc::Error>(string)
//! });
//!
//! // Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
//! // the task above:
//! let (tx, rx) = c2.split();
//! let send_string = tokio::spawn(async move {
//!     let tx = tx.send("Hello!".to_string()).await?;
//!     Ok::<_, mpsc::Error>(tx)
//! });
//! let recv_vec = tokio::spawn(async move {
//!     let (vec, rx) = rx.recv().await?;
//!     Ok::<_, mpsc::Error>((vec, rx))
//! });
//!
//! // Wait for the threads to finish
//! let tx = send_string.await??;
//! let (vec, rx) = recv_vec.await??;
//! let string = t1.await??;
//!
//! // Unsplit the ends of `c2`
//! let c2 = Chan::unsplit(tx, rx).unwrap();
//!
//! assert_eq!(vec, &[1, 2, 3, 4, 5]);
//! assert_eq!(string, "Hello!");
//! # Ok(())
//! # }
//! ```
//!
//! When using [`Split`], keep in mind its limitations:
//!
//! - It's a type error to [`Send`] or [`Choose`] on the receive-only end.
//! - It's a type error to [`Recv`] or [`Offer`] on the transmit-only end.
//! - You can [`unsplit`](Chan::unsplit) the two ends again only once their session types match each
//!   other.
//! - It's a runtime [`UnsplitError`] to attempt to [`unsplit`](Chan::unsplit) two [`Chan`]s which
//!   did not originate from the same call to [`split`](Chan::split), even if their types match.
//!
//! # Wrapping up
//!
//! We've now finished our tour of everything you need to get started programming with Dialectic! ✨
//!
//! You might now want to...
//!
//! - Check out the documentation for **[`Chan`]**, if you haven't already?
//! - Learn how to instantiate (or implement) a **[`backend`]** other than the
//!   **[`mpsc`](backend::mpsc)** backend we used in the tutorial above?
//! - Jump back to the top of **[reference documentation](crate#quick-reference)**?
//!
//! Thanks for following along, and enjoy!

// Import the whole crate so the docs above can link appropriately.
#[allow(unused_imports)]
use crate::*;
