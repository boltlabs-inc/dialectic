//! # Dialectic
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
//!   tour of the crate](#getting-started)**.
//! - If you're **familiar with session types**, you might jump to the **[quick
//!   reference](#quick-reference)**, then read more in the [`types`](crate::types) module and the
//!   documentation for [`Chan`](crate::Chan).
//! - If you want to **integrate your own channel type** with Dialectic, you need to implement the
//!   [`Transmit`] and [`Receive`] traits from the [`backend`] module.
//! - Or, you can **[dive into the reference documentation](#modules)**...
//!
//! # Getting started
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
//! type JustSendOneString = Send<String, End>;
//! ```
//!
//! This type specifies a very simple protocol: "send a string, then finish." The first argument to
//! `Send` is the type sent, and the second is the rest of the protocol, which in this case is the
//! empty protocol [`End`].
//!
//! Every session type has a [`Dual`](crate::Session::Dual), which describes what the other end of
//! the channel must do to follow the channel's protocol; if one end [`Send`]s, the other end must
//! [`Recv`]:
//!
//! ```
//! # use dialectic::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(<Send<String, End> as Session>::Dual, Recv<String, End>);
//! ```
//!
//! Because many sessions end with either a `Send` or a `Recv`, their types can be abbreviated when
//! the rest of the session is `End`:
//!
//! ```
//! # use dialectic::*;
//! # use static_assertions::assert_type_eq_all;
//! assert_type_eq_all!(Send<String>, Send<String, End>);
//! assert_type_eq_all!(Recv<String>, Recv<String, End>);
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
//! # type JustSendOneString = Send<String, End>;
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! ```
//!
//! The types of `c1` and `c2` are inferred from the session type specified: `c1`'s type corresponds
//! to the given session type, and `c2`'s type corresponds to its dual:
//!
//! ```
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, End>;
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
//! # type JustSendOneString = Send<String, End>;
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
//! # type JustSendOneString = Send<String, End>;
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
//! type JustSendOneString = Send<String, End>;
//! ```
//!
//! Trying to do any of the below things results in a compile-time type error (edited for brevity).
//!
//! If we try to send on the end of the channel that's meant to receive...
//!
//! ```compile_fail
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, End>;
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
//! # type JustSendOneString = Send<String, End>;
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
//! # type JustSendOneString = Send<String, End>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
//! let c1 = c1.send("Hello, world!".to_string()).await?;
//! c1.send("Hello again!".to_string()).await?;
//! # Ok(())
//! # }
//! ```
//!
//! ...we get an error we saying the returned channel is now of type `Chan<_, _, End>`, and we can't
//! send when it's the end:
//!
//! ```text
//! error[E0599]: no method named `send` found for struct `Chan<Sender, Receiver, End>` in the current scope
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
//! channel must indicate a choice of which protocol to follow -- and we need to handle the result
//! of either selection by enacting the protocol chosen.
//!
//! ```
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type JustSendOneString = Send<String, End>;
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use dialectic::constants::*;
//!
//! let (c1, c2) = <Offer<(Send<i64>, Recv<String>)>>::channel(|| mpsc::channel(1));
//!
//! // Offer a choice
//! let t1 = tokio::spawn(async move {
//!     let c1 = offer!(c1 =>
//!         { c1.send(42).await? },  // handle `c2.choose(_0)`
//!         { c1.recv().await?.1 },  // handle `c2.choose(_1)`
//!     );
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
//! (this must be an identifier, not an expression) and a list of expressions which may use that
//! channel. In each expression, the channel's type corresponds to the session type for that choice.
//! The type of each expression in the list must be the same, which means that if we want to bind a
//! channel name to the result of the `offer!`, each expression must step the channel forward to an
//! identical session type (in the case above, that's `End`).
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
//! could describe this protocol using the [`Loop`] and [`Recur`] types:
//!
//! ```
//! # use dialectic::*;
//! type QuerySum = Loop<Choose<(Send<i64, Recur>, Recv<i64>)>>;
//! ```
//!
//! The dual to `Loop<P>` is `Loop<P::Dual>`, and the dual to `Recur` is `Recur`, so we know the
//! other end of this channel will need to implement:
//!
//! ```
//! # use dialectic::*;
//! # use static_assertions::assert_type_eq_all;
//! # type QuerySum = Loop<Choose<(Send<i64, Recur>, Recv<i64>)>>;
//! type ComputeSum = Loop<Offer<(Recv<i64, Recur>, Send<i64>)>>;
//! assert_type_eq_all!(<QuerySum as Session>::Dual, ComputeSum);
//! ```
//!
//! We can implement this protocol by following the session types. When the session type of a
//! [`Chan`] hits a [`Recur`] point, it jumps back to the type of the [`Loop`] to which that
//! [`Recur`] refers. In this case, for example, immediately after the querying task sends an
//! integer, the resultant channel will have the session type `Choose<(Send<i64, Recur>,
//! Recv<i64>)>` once more.
//!
//! ```
//! # use dialectic::*;
//! # use dialectic::backend::mpsc;
//! # type QuerySum = Loop<Choose<(Send<i64, Recur>, Recv<i64>)>>;
//! # type ComputeSum = Loop<Offer<(Recv<i64, Recur>, Send<i64>)>>;
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
//!         c2 = offer!(c2 =>
//!             {
//!                 let (n, c2) = c2.recv().await?;
//!                 sum += n;
//!                 c2
//!             },
//!             { break c2; },
//!         );
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
//! the optional parameter of `Recur`. By default, `Recur` jumps to the innermost loop; however,
//! `Recur<_1>` jumps to the second-innermost, `Recur<_2>` the third-innermost, etc. The types
//! [`_0`](unary::types::_0), [`_1`](unary::types::_1), [`_2`](unary::types::_2), etc. are defined
//! in the [`unary::types`] module.
//!
//! ## Automatic looping
//!
//! You may have noticed how in the example above, [`choose`](Chan::choose) can be called on `c1`
//! even though the outermost part of `c1`'s session type `QuerySum` would seem not to begin with
//! [`Choose`]. This is true in general: if the session type of a [`Chan`] either [`Loop`]s or
//! [`Recur`]s to a session type for which a given operation is valid, that operation is valid on
//! the [`Chan`]. In the instance above, calling [`choose`](Chan::choose) on a [`Chan`] with session
//! type `Loop<Choose<...>>` works, no matter how many `Loop`s enclose the `Choose`. Similarly, if a
//! `Chan`'s type is `Recur`, whatever operation would be valid for the session type at the start of
//! the corresponding `Loop` is valid for that `Chan`.
//!
//! This behavior is enabled by the [`Actionable`] trait, which defines what the next "real action"
//! on a session type is. For [`End`], [`Send`], [`Recv`], [`Offer`], [`Choose`], and [`Split`] (the
//! final session type discussed below), the "real action" is that session type itself. However, for
//! [`Loop`] and [`Recur`], the next action is whatever follows entering the loop(s) or recurring,
//! respectively.
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
//! # Quick reference
//!
//! The tutorial above covers all the constructs necessary to write session-typed programs with
//! Dialectic. A quick summary:
//!
//! - To make a pair of dual [`Chan`]s for a session type `P`: [`let (c1, c2) = P::channel(||
//!   {...})`](NewSession::channel) with some closure that builds a unidirectional underlying
//!   channel.
//! - To wrap an existing sender `tx` and receiver `rx` in a single [`Chan`] for `P`: [`let c =
//!   P::wrap(tx, rx)`](NewSession::wrap).
//!
//! Once you've got a channel, here's what you can do:
//!
//! | Session Type | Channel Operation(s) | Dual Type |
//! | :----------- | :------------------- | :-------- |
//! | [`Send<T, P = End>`](Send) | [`let c = c.send(t: T).await?;`](Chan::send) | [`Recv<T, P::Dual>`](Recv) |
//! | [`Recv<T, P = End>`](Recv) | [`let (t, c) = c.recv().await?;`](Chan::recv) | [`Send<T, P::Dual>`](Send) |
//! | [`Choose<Choices>`](Choose) | [`let c = c.choose(_N).await?;`](Chan::choose) | [`Offer<Choices::Dual>`](Offer) |
//! | [`Offer<Choices>`](Offer) | [`let c = offer!(c => {...}, ...);`](offer) | [`Choose<Choices::Dual>`](Choose) |
//! | [`Split<P, Q>`](Split) | [`let (tx, rx) = c.split();`](Chan::split)<br>`// concurrently use tx and rx`<br>[`let c = Chan::unsplit(tx, rx)?;`](Chan::unsplit) | [`Split<Q::Dual, P::Dual>`](Split) |
//! | [`Loop<P>`](Loop) | (none) | [`Loop<P::Dual>`](Loop) |
//! | [`Recur<N = Z>`](Recur) | (none) | [`Recur<N>`](Recur) |
//! | [`End`] | [`let (tx, rx) = c.close();`](Chan::close) | [`End`] | [`c.close()`](Chan::close) |

#![recursion_limit = "256"]
#![allow(clippy::type_complexity)]
#![cfg_attr(docsrs, feature(doc_cfg))]
use std::{
    marker::{self, PhantomData},
    sync::Arc,
};
use thiserror::Error;

use crate::backend::*;
use tuple::{List, Tuple};
pub use types::*;

pub mod backend;
pub mod types;

/// A bidirectional communications channel sending outgoing messages via the `Tx` connection and
/// receiving incoming messages via the `Rx` connection, whose future session behavior is determined
/// by the session type `P`. The `E` parameter is a type-level list describing the *session
/// environment* of the channel: the stack of [`Loop`]s the channel has entered.
///
/// # Creating new `Chan`s: use [`NewSession`]
///
/// To construct a new [`Chan`], use one of the static methods of [`NewSession`] on the session type
/// for which you want to create a channel. Here, we create two `Chan`s with the session type
/// `Send<String>` and its dual `Recv<String>`, wrapping an underlying bidirectional transport built
/// from a pair of [`tokio::sync::mpsc::channel`]s:
///
/// ```
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Make a pair of channels:
/// // - `c1` with the session type `Send<String>`, and
/// // - `c2` with the dual session type `Recv<String>`
/// let (c1, c2) = <Send<String>>::channel(|| backend::mpsc::channel(1));
/// # Ok(())
/// # }
/// ```
///
/// If you already have a sender and receiver and want to wrap them in a `Chan`, use the
/// [`wrap`](NewSession::wrap) method for a session type. This is useful, for example, if you're
/// talking to another process over a network connection, where it's not possible to build both
/// halves of the channel on one computer, and instead each computer will wrap one end of the
/// connection:
///
/// ```
/// # use dialectic::*;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let (tx, rx) = backend::mpsc::channel(1);
/// let c = <Send<String>>::wrap(tx, rx);
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
#[must_use = "Dropping a channel before finishing its session type will result in a panic"]
pub struct Chan<Tx, Rx, P: Actionable<E>, E: Environment = ()>
where
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    tx: Tx,
    rx: Rx,
    environment: PhantomData<E>,
    protocol: PhantomData<P>,
}

/// The `NewSession` extension trait gives methods to create session-typed channels from session
/// types. These are implemented as static methods on the session type itself. For instance:
///
/// ```
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Send<String>>::channel(backend::mpsc::unbounded_channel);
/// // do something with these channels...
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
///
/// # Notes
///
/// The trait bounds specified for [`NewSession`] ensure that the session type is well-formed.
/// However, it does not ensure that all the types in the session type can be sent or received over
/// the given channels.
///
/// Valid session types must contain only "productive" recursion: they must not contain any
/// [`Recur`] directly inside the loop to which that recursion refers. For instance, attempting to
/// use the session type `Loop<Recur>` will result in a compile-time trait solver overflow.
///
/// ```compile_fail
/// use dialectic::*;
///
/// # #[tokio::main]
/// # async fn main() {
/// let (c1, c2) = <Loop<Recur>>::channel(backend::mpsc::unbounded_channel);
/// #   c1.unwrap();
/// #   c2.unwrap();
/// # }
/// ```
///
/// This results in the compiler error:
///
///
/// ```text
/// error[E0275]: overflow evaluating the requirement `Recur: Actionable<(Recur, ())>`
///   |
/// 7 | let (c1, c2) = <Loop<Recur>>::channel(backend::mpsc::unbounded_channel);
///   |                ^^^^^^^^^^^^^^^^^^^^^^
///   |
///   = help: consider adding a `#![recursion_limit="256"]` attribute to your crate
///   = note: required because of the requirements on the impl of `Actionable<()>` for `Loop<Recur>`
///   = note: required because of the requirements on the impl of `NewSession` for `Loop<Recur>`
/// ```
///
/// In this situation, you **should not** take `rustc`'s advice. If you add a
/// `#![recursion_limit="256"]` attribute to your crate: this will only cause the compiler to work
/// harder before giving you the same error!
///
/// What the compiler is trying to tell you is that the session type as specified does not
/// correspond to a valid sequence of actions on the channel, because it contains unproductive
/// recursion.
pub trait NewSession
where
    Self: Actionable<()>,
    Self::Dual: Actionable<()>,
    <Self::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
{
    /// Given a closure which generates a uni-directional underlying transport channel, create a
    /// pair of dual [`Chan`]s which communicate over the transport channels resulting from these
    /// closures.
    ///
    /// By internally wiring together the two directional channels, this function assures that
    /// communications over the channels actually follow the session specified.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = End::channel(backend::mpsc::unbounded_channel);
    /// # }
    /// ```
    fn channel<Tx, Rx>(
        make: impl FnMut() -> (Tx, Rx),
    ) -> (
        Chan<Tx, Rx, Self::Action, Self::Env>,
        Chan<Tx, Rx, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    );

    /// Given two closures, each of which generates a uni-directional underlying transport channel,
    /// create a pair of dual [`Chan`]s which communicate over the transport channels resulting from
    /// these closures.
    ///
    /// By internally wiring together the two directional channels, this function assures that
    /// communications over the channels actually follow the session specified.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = End::bichannel(
    ///     backend::mpsc::unbounded_channel,
    ///     || backend::mpsc::channel(1),
    /// );
    /// # }
    /// ```
    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self::Action, Self::Env>,
        Chan<Tx1, Rx0, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    );

    /// Given a transmitting and receiving end of an un-session-typed connection, wrap them in a new
    /// session-typed channel for the protocol `P.`
    ///
    /// It is expected that the other ends of these connections will be wrapped in a channel with
    /// the [`Dual`](crate::Session::Dual) session type.
    ///
    /// # Notes
    ///
    /// Because `&mut Tx` and `&mut Rx` are [`Transmit`] and [`Receive`] if `Tx` and `Rx` are
    /// [`Transmit`] and `Receive` respectively, a [`Chan`] does not need to own its connections; a
    /// mutable reference or an owned type work equally well as inputs to `wrap`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (mut tx, mut rx) = backend::mpsc::unbounded_channel();
    /// let c = End::wrap(&mut tx, &mut rx);  // you can wrap &mut references
    /// c.close();                            // whose lifetimes end when the channel is closed,
    /// let c = End::wrap(tx, rx);            // or you can wrap owned values
    /// let (tx, rx) = c.close();             // and get them back when the channel is closed.
    /// # }
    /// ```
    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self::Action, Self::Env>;
}

impl<P> NewSession for P
where
    Self: Actionable<()>,
    Self::Dual: Actionable<()>,
    <Self::Env as EachSession>::Dual: Environment,
    <<Self::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
{
    fn channel<Tx, Rx>(
        mut make: impl FnMut() -> (Tx, Rx),
    ) -> (
        Chan<Tx, Rx, Self::Action, Self::Env>,
        Chan<Tx, Rx, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    ) {
        let (tx0, rx0) = make();
        let (tx1, rx1) = make();
        (P::wrap(tx0, rx1), <P::Dual>::wrap(tx1, rx0))
    }

    fn bichannel<Tx0, Rx0, Tx1, Rx1>(
        make0: impl FnOnce() -> (Tx0, Rx0),
        make1: impl FnOnce() -> (Tx1, Rx1),
    ) -> (
        Chan<Tx0, Rx1, Self::Action, Self::Env>,
        Chan<Tx1, Rx0, <Self::Dual as Actionable<()>>::Action, <Self::Dual as Actionable<()>>::Env>,
    ) {
        let (tx0, rx0) = make0();
        let (tx1, rx1) = make1();
        (P::wrap(tx0, rx1), <P::Dual>::wrap(tx1, rx0))
    }

    fn wrap<Tx, Rx>(tx: Tx, rx: Rx) -> Chan<Tx, Rx, Self::Action, Self::Env> {
        unsafe { Chan::with_env(tx, rx) }
    }
}

impl<Tx, Rx, P, E> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = End>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Close a finished session, returning the wrapped connections used during the session.
    ///
    /// This function does not do cleanup on the actual underlying connections; they are passed back
    /// to the caller who may either continue to use them outside the session typing context or
    /// clean them up as appropriate.
    ///
    /// # Examples
    ///
    /// Starting with a channel whose session type is already `End`, we can immediately close the
    /// channel.
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = End::channel(backend::mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.close();
    /// let (tx2, rx2) = c2.close();
    /// # }
    /// ```
    ///
    /// However, if the channel's session type is *not* `End`, it is a type error to attempt to
    /// close the channel and retrieve its underlying sender and receiver. The following code **will
    /// not compile**:
    ///
    /// ```compile_fail
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let (c1, c2) = <Loop<Send<String, Recur>>>::channel(backend::mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.close();
    /// let (tx2, rx2) = c2.close();
    /// # }
    /// ```
    ///
    /// If you *really* want to destruct a channel before the end of its session, use
    /// [`unwrap`](Chan::unwrap), but beware that this may cause the party on the other end of the
    /// channel to throw errors due to your violation of the channel's protocol!
    pub fn close(self) -> (Tx, Rx) {
        self.unwrap()
    }
}

impl<Tx, Rx, E, P, T, Q> Chan<Tx, Rx, P, E>
where
    Rx: Receive<T>,
    T: marker::Send + 'static,
    P: Actionable<E, Action = Recv<T, Q>>,
    Q: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <Q::Env as EachSession>::Dual: Environment,
    <<Q::Action as Actionable<Q::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Receive something of type `T` on the channel, returning the pair of the received object and
    /// the channel.
    ///
    /// # Errors
    ///
    /// This function returns the [`Receive::Error`] for the underlying `Rx` connection if there was
    /// an error while receiving.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Recv<String>>::channel(|| backend::mpsc::channel(1));
    /// c2.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c1) = c1.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    pub async fn recv(mut self) -> Result<(T, Chan<Tx, Rx, Q::Action, Q::Env>), Rx::Error> {
        match self.rx.recv().await {
            Ok(result) => Ok((result, unsafe { self.cast() })),
            Err(err) => Err(err),
        }
    }
}

impl<'a, Tx, Rx, E, P, T, Q> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Send<T, Q>>,
    Q: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <Q::Env as EachSession>::Dual: Environment,
    <<Q::Action as Actionable<Q::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Send something of type `T` on the channel, returning the channel.
    ///
    /// The underlying sending channel `Tx` may be able to send a `T` using multiple different
    /// [`CallingConvention`]s: by [`Val`], by [`Ref`] and/or by [`Mut`]. To disambiguate, use
    /// "turbofish" syntax when calling `send`, i.e. `chan.send::<Val>(1)` or
    /// `chan.send::<Ref>(&true)`.
    ///
    /// # Errors
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Send<String>>::channel(|| backend::mpsc::channel(1));
    /// c1.send("Hello, world!".to_string()).await?;
    ///
    /// let (s, c2) = c2.recv().await?;
    /// assert_eq!(s, "Hello, world!");
    /// # Ok(())
    /// # }
    /// ```
    pub async fn send<Convention: CallingConvention>(
        mut self,
        message: <T as CallBy<'a, Convention>>::Type,
    ) -> Result<Chan<Tx, Rx, Q::Action, Q::Env>, <Tx as Transmit<'a, T, Convention>>::Error>
    where
        Tx: Transmit<'a, T, Convention>,
        T: CallBy<'a, Convention>,
        <T as CallBy<'a, Convention>>::Type: marker::Send,
    {
        match self.tx.send(message).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => Err(err),
        }
    }
}

impl<Tx, Rx, E, P, Choices> Chan<Tx, Rx, P, E>
where
    Tx: Transmit<'static, u8, Val>,
    P: Actionable<E, Action = Choose<Choices>>,
    Choices: Tuple,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    Choices::AsList: EachScoped<<P::Env as Environment>::Depth>,
{
    /// Actively choose to enter the `N`th protocol offered via [`offer`](Chan::offer) by the other end of
    /// the connection, alerting the other party to this choice by sending the number `N` over the
    /// channel.
    ///
    /// The choice `N` is specified as a type-level [`Unary`] number. Predefined constants for all
    /// supported numbers of choices (up to a maximum of 127) are available in the [`constants`]
    /// module, each named for its corresponding decimal number prefixed with an underscore (e.g.
    /// `_0`, or `_42`).
    ///
    /// # Errors
    ///
    /// This function returns the [`Transmit::Error`] for the underlying `Tx` connection if there
    /// was an error while sending the choice.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
    ///
    /// let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
    ///
    /// // Spawn a thread to offer a choice
    /// let t1 = tokio::spawn(async move {
    ///     offer! { c2 =>
    ///         { c2.recv().await?; },
    ///         { c2.send("Hello!".to_string()).await?; },
    ///     }
    ///     Ok::<_, backend::mpsc::Error>(())
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
    ///
    /// Attempting to choose an index that's out of bounds results in a compile-time error:
    ///
    /// ```compile_fail
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type OnlyTwoChoices = Choose<(End, End)>;
    /// let (c1, c2) = OnlyTwoChoices::channel(|| backend::mpsc::channel(1));
    ///
    /// // Try to choose something out of range (this doesn't typecheck)
    /// c1.choose(_2).await?;
    ///
    /// # // Wait for the offering thread to finish
    /// # t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn choose<N: Unary>(
        mut self,
        _choice: N,
    ) -> Result<
        Chan<
            Tx,
            Rx,
            <<Choices::AsList as Select<N>>::Selected as Actionable<E>>::Action,
            <<Choices::AsList as Select<N>>::Selected as Actionable<E>>::Env,
        >,
        Tx::Error,
    >
    where
        N: LessThan<unary::types::_128>,
        Choices::AsList: Select<N>,
        <Choices::AsList as Select<N>>::Selected: Actionable<E>,
        <<<Choices::AsList as Select<N>>::Selected as Actionable<E>>::Env as EachSession>::Dual:
            Environment,
    {
        match self.tx.send(N::VALUE as u8).await {
            Ok(()) => Ok(unsafe { self.cast() }),
            Err(err) => Err(err),
        }
    }
}

/// The result of [`offer`](Chan::offer), `Branches<Tx, Rx, Ps>` represents an enumeration of the possible
/// new channel states that could result from the offering of the type-level list of protocols `Ps`.
///
/// To find out which protocol was selected by the other party, use [`Branches::case`], or better
/// yet, use the [`offer!`](crate::offer) macro to ensure you don't miss any cases.
#[derive(Debug)]
#[must_use]
pub struct Branches<Tx, Rx, Choices, E = ()>
where
    Choices: Tuple,
    Choices::AsList: EachActionable<E>,
    E: Environment,
    E::Dual: Environment,
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
    E::Dual: Environment,
    P::Dual: Actionable<E::Dual>,
    <P::Env as EachSession>::Dual: Environment,
    <<P::Action as Actionable<P::Env>>::Env as EachSession>::Dual: Environment,
    <<P::Dual as Actionable<E::Dual>>::Env as EachSession>::Dual: Environment,
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
            Ok(unsafe { Chan::with_env(tx, rx) })
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

/// An error representing a protocol violation where the other end of the channel has selected a
/// choice whose index is too high to be handled by the current session type.
///
/// For instance, if the current channel's session type is `Offer<(..., (..., (..., ())))>`, then
/// this error is thrown if an invocation of `offer!` encounters a discriminant sent from the other
/// side which is greater than `2`.
///
/// Authors of backends may wish to write a `From<OutOfRangeChoice>` implementation for their
/// backend's error type so that these errors are automatically handled.
#[derive(Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[error("received discriminant out of range in `Chan::offer`")]
pub struct OutOfRangeChoice;

impl<'a, Tx, Rx, E: Environment> Branches<Tx, Rx, (), E>
where
    E::Dual: Environment,
{
    /// Attempt to eliminate an empty [`Branches`], returning an error if the originating
    /// discriminant for this set of protocol choices was out of range.
    ///
    /// This function is only callable on empty [`Branches`], which under ordinary circumstances
    /// means it proves the unreachability of its calling location. However, if the other end of the
    /// channel breaks protocol, an empty [`Branches`] can in fact be constructed, and this function
    /// will then signal an error.
    pub fn empty_case<T>(self) -> Result<T, OutOfRangeChoice> {
        Err(OutOfRangeChoice)
    }
}

impl<'a, Tx, Rx, E, P, Choices> Chan<Tx, Rx, P, E>
where
    Rx: Receive<u8>,
    P: Actionable<E, Action = Offer<Choices>>,
    Choices: Tuple,
    <Choices::AsList as EachSession>::Dual: List,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    Choices::AsList: EachActionable<P::Env>,
    Choices::AsList: EachScoped<<P::Env as Environment>::Depth>,
{
    /// Offer the choice of one or more protocols to the other party, and wait for them to indicate
    /// by sending a number which protocol to proceed with.
    ///
    /// # Notes
    ///
    /// **Where possible, prefer the [`offer!`](crate::offer) macro**. This has the benefit of
    /// ensuring at compile time that no case is left unhandled; it's also more succinct.
    ///
    /// # Errors
    ///
    /// This function returns the [`Receive::Error`] for the underlying `Rx` connection if there was
    /// an error while receiving.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
    ///
    /// let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
    ///
    /// // Spawn a thread to offer a choice
    /// let t1 = tokio::spawn(async move {
    ///     match c2.offer().await?.case() {
    ///         Ok(c2) => { c2.recv().await?; },
    ///         Err(rest) => match rest.case() {
    ///             Ok(c2) => { c2.send("Hello!".to_string()).await?; },
    ///             Err(rest) => rest.empty_case()?,
    ///         }
    ///     }
    ///     Ok::<_, backend::mpsc::Error>(())
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
    ///
    /// Notice how the handling of cases by manual `match` is harder to read than the equivalent in
    /// terms of [`offer!`](crate::offer):
    ///
    /// ```
    /// # use dialectic::*;
    /// # use dialectic::constants::*;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
    /// #
    /// # let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
    /// #
    /// # // Spawn a thread to offer a choice
    /// # let t1 = tokio::spawn(async move {
    /// offer! { c2 =>
    ///     { c2.recv().await?; },
    ///     { c2.send("Hello!".to_string()).await?; },
    /// }
    /// # Ok::<_, backend::mpsc::Error>(())
    /// # });
    /// #
    /// # // Choose to send an integer
    /// # c1.choose(_0).await?.send(42).await?;
    /// #
    /// # // Wait for the offering thread to finish
    /// # t1.await??;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn offer(self) -> Result<Branches<Tx, Rx, Choices, P::Env>, Rx::Error> {
        let (tx, mut rx) = self.unwrap();
        match rx.recv().await {
            Ok(variant) => Ok(Branches {
                variant,
                tx,
                rx,
                protocols: PhantomData,
                environment: PhantomData,
            }),
            Err(err) => Err(err),
        }
    }
}

/// Offer a set of different protocols, allowing the other side of the channel to choose with which
/// one to proceed. This macro only works in a `Try` context, i.e. somewhere the `?` operator would
/// make sense to use.
///
/// # Notes
///
/// - You must specify exactly as many branches as there are options in the type of the
///   [`Offer`](crate::types::Offer) to which this expression corresponds.
/// - In the body of each branch, the identifier for the channel is rebound to have the session type
///   corresponding to that branch.
/// - To use `offer!` as an expression, ensure the type of every branch matches.
///
/// # Examples
///
/// ```
/// use dialectic::*;
/// use dialectic::constants::*;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
///
/// let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
///
/// // Spawn a thread to offer a choice
/// let t1 = tokio::spawn(async move {
///     offer! { c2 =>
///         { c2.recv().await?; },
///         { c2.send("Hello!".to_string()).await?; },
///     }
///     Ok::<_, backend::mpsc::Error>(())
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
///
/// # Errors
///
/// The generated code may result in the [`Transmit::Error`] of the underlying connection, as well
/// as [`OutOfRangeChoice`] in the case where the other side of the connection breaks the protocol
/// and selects a choice that does not exist. If the other side of the channel is also using this
/// session type, that case is impossible.
///
/// Alternatively, you may specify an optional clause to handle protocol violations where the other
/// side signals an invalid choice. In the example above, we could choose to immediately panic in
/// that scenario:
///
/// ```
/// # use dialectic::*;
/// # use dialectic::constants::*;
/// #
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # type GiveOrTake = Choose<(Send<i64>, Recv<String>)>;
/// #
/// # let (c1, c2) = GiveOrTake::channel(|| backend::mpsc::channel(1));
/// #
/// # // Spawn a thread to offer a choice
/// # let t1 = tokio::spawn(async move {
/// offer! { c2 =>
///     { c2.recv().await?; },
///     { c2.send("Hello!".to_string()).await?; },
///     ? => panic!("Protocol violation!"),
/// }
/// # Ok::<_, backend::mpsc::Error>(())
/// # });
/// #
/// # // Choose to send an integer
/// # c1.choose(_0).await?.send(42).await?;
/// #
/// # // Wait for the offering thread to finish
/// # t1.await??;
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! offer {
    (
        $chan:ident => $($t:tt)*
    ) => (
        {
            match $crate::Chan::offer($chan).await {
                Ok(b) => $crate::offer!{@branches b, $chan, $($t)* },
                Err(e) => Err(e)?,
            }
        }
    );
    (
        @branches $branch:ident, $chan:ident, $code:expr $(,)?
    ) =>
    (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => $code,
            std::result::Result::Err($branch) => $crate::Branches::empty_case($branch)?,
        }
    );
    (
        @branches $branch:ident, $chan:ident, $code:expr, $($t:tt)+
    ) => (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => $code,
            std::result::Result::Err($branch) => $crate::offer!{@branches $branch, $chan, $($t)+ },
        }
    );
    (
        @branches $branch:ident, $chan:ident, ? => $empty:expr $(,)?
    ) => (
        {
            let _: $crate::Branches<_, _, (), _> = $branch;
            $empty
        }
    );
}

/// A placeholder for a missing transmit or receive end of a connection.
///
/// When using [`split`](Chan::split), the resultant two channels can only send or only receive,
/// respectively. This is reflected at the type level by the presence of `Unavailable` on the type
/// of the connection which is not present for each part of the split.
#[derive(Debug)]
pub struct Unavailable<T>(Arc<()>, PhantomData<T>);

impl<T> Clone for Unavailable<T> {
    fn clone(&self) -> Self {
        Unavailable(Arc::clone(&self.0), PhantomData)
    }
}

impl<T> Unavailable<T> {
    /// Make a new `Unavailable`.
    fn new() -> Self {
        Unavailable(Arc::new(()), PhantomData)
    }

    /// Cast an `Unavailable` to a new phantom type.
    fn cast<S>(self) -> Unavailable<S> {
        Unavailable(self.0, PhantomData)
    }
}

impl<Tx, Rx, E, P, Q, R> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = Split<Q, R>>,
    Q: Actionable<P::Env>,
    R: Actionable<P::Env>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <<<Q as Actionable<P::Env>>::Action as Actionable<<Q as Actionable<P::Env>>::Env>>::Env as EachSession>::Dual: Environment,
    <<<R as Actionable<P::Env>>::Action as Actionable<<R as Actionable<P::Env>>::Env>>::Env as EachSession>::Dual: Environment,
{
    /// Split a channel into transmit-only and receive-only ends which may be used concurrently and
    /// reunited (provided they reach a matching session type) using [`unsplit`](Chan::unsplit).
    ///
    /// # Examples
    ///
    /// In this example, both ends of a channel concurrently interact with its split send/receive
    /// halves. If the underlying channel implementation allows for parallelism, this simultaneous
    /// interaction can be faster than sequentially sending data back and forth.
    ///
    /// ```
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// type SendAndRecv = Split<Send<Vec<usize>>, Recv<String>>;
    ///
    /// let (c1, c2) = SendAndRecv::channel(|| backend::mpsc::channel(1));
    ///
    /// // Spawn a thread to simultaneously send a `Vec<usize>` and receive a `String`:
    /// let t1 = tokio::spawn(async move {
    ///     let (tx, rx) = c1.split();
    ///     let send_vec = tokio::spawn(async move {
    ///         tx.send(vec![1, 2, 3, 4, 5]).await?;
    ///         Ok::<_, backend::mpsc::Error>(())
    ///     });
    ///     let recv_string = tokio::spawn(async move {
    ///         let (string, _) = rx.recv().await?;
    ///         Ok::<_, backend::mpsc::Error>(string)
    ///     });
    ///     send_vec.await.unwrap()?;
    ///     let string = recv_string.await.unwrap()?;
    ///     Ok::<_, backend::mpsc::Error>(string)
    /// });
    ///
    /// // Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
    /// // the task above:
    /// let (tx, rx) = c2.split();
    /// let send_string = tokio::spawn(async move {
    ///     tx.send("Hello!".to_string()).await?;
    ///     Ok::<_, backend::mpsc::Error>(())
    /// });
    /// let recv_vec = tokio::spawn(async move {
    ///     let (vec, _) = rx.recv().await?;
    ///     Ok::<_, backend::mpsc::Error>(vec)
    /// });
    ///
    /// // Examine the result values:
    /// send_string.await??;
    /// let vec = recv_vec.await??;
    /// let string = t1.await??;
    /// assert_eq!(vec, &[1, 2, 3, 4, 5]);
    /// assert_eq!(string, "Hello!");
    /// # Ok(())
    /// # }
    /// ```
    pub fn split(
        self,
    ) -> (
        Chan<Tx, Unavailable<Rx>, Q::Action, Q::Env>,
        Chan<Unavailable<Tx>, Rx, R::Action, R::Env>,
    )
    {
        let (tx, rx) = self.unwrap();
        let unavailable = Unavailable::new();
        let tx_only = unsafe { Chan::with_env(tx, unavailable.clone()) };
        let rx_only = unsafe { Chan::with_env(unavailable.cast(), rx) };
        (tx_only, rx_only)
    }
}

/// The error returned when two channels cannot be [`unsplit`](Chan::unsplit) together because they
/// originate from different channels contains the two channels which couldn't be reunited.
#[derive(Debug)]
pub struct UnsplitError<Tx, Rx, P, Q, E, F>
where
    P: Actionable<E>,
    Q: Actionable<F>,
    E: Environment,
    F: Environment,
    E::Dual: Environment,
    F::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <Q::Env as EachSession>::Dual: Environment,
{
    /// The transmit-only half.
    pub tx: Chan<Tx, Unavailable<Rx>, P, E>,

    /// The receive-only half.
    pub rx: Chan<Unavailable<Tx>, Rx, Q, F>,
}

impl<Tx, Rx, P, Q, E, F> std::error::Error for UnsplitError<Tx, Rx, P, Q, E, F>
where
    Tx: std::fmt::Debug,
    Rx: std::fmt::Debug,
    P: Actionable<E> + std::fmt::Debug,
    Q: Actionable<F> + std::fmt::Debug,
    E: Environment + std::fmt::Debug,
    F: Environment + std::fmt::Debug,
    E::Dual: Environment,
    F::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <Q::Env as EachSession>::Dual: Environment,
{
}

impl<Tx, Rx, P, Q, E, F> std::fmt::Display for UnsplitError<Tx, Rx, P, Q, E, F>
where
    P: Actionable<E>,
    Q: Actionable<F>,
    E: Environment,
    F: Environment,
    E::Dual: Environment,
    F::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
    <Q::Env as EachSession>::Dual: Environment,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "cannot `Chan::unsplit` two unrelated channels")
    }
}

impl<Tx, Rx, E, P> Chan<Tx, Rx, P, E>
where
    P: Actionable<E, Action = P, Env = E>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Reunite the transmit-only and receive-only channels resulting from a call to [`split`](Chan::split)
    /// into a single channel.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    /// use dialectic::constants::*;
    ///
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let (c1, c2) = <Split<End, End>>::channel(|| backend::mpsc::channel(1));
    /// let (tx1, rx1) = c1.split();
    /// let (tx2, rx2) = c2.split();
    ///
    /// let c1 = Chan::unsplit(tx1, rx1)?;
    /// let c2 = Chan::unsplit(tx2, rx2)?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// If the two channels given as input did not result from the same call to [`split`](Chan::split),
    /// this function returns an [`UnsplitError`] containing the two channels, since rewiring
    /// channels to be non-bidirectional can violate the session typing guarantee. If instead of the
    /// above, we did the following, `unsplit` would return an error:
    ///
    /// ```
    /// # use dialectic::*;
    /// # use dialectic::constants::*;
    /// #
    /// # #[tokio::main]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # type SplitEnds = Split<End, End>;
    /// #
    /// let (c1, c2) = SplitEnds::channel(|| backend::mpsc::channel(1));
    /// let (tx1, rx1) = c1.split();
    /// let (tx2, rx2) = c2.split();
    ///
    /// assert!(Chan::unsplit(tx1, rx2).is_err()); // <-- pairing tx from c1 with rx from c2
    /// assert!(Chan::unsplit(tx2, rx1).is_err()); // <-- pairing tx from c2 with rx from c1
    /// # Ok(())
    /// # }
    /// ```
    pub fn unsplit<Q, R, F, G>(
        tx: Chan<Tx, Unavailable<Rx>, Q, F>,
        rx: Chan<Unavailable<Tx>, Rx, R, G>,
    ) -> Result<Self, UnsplitError<Tx, Rx, Q, R, F, G>>
    where
        Q: Actionable<F, Action = P, Env = E>,
        R: Actionable<G, Action = P, Env = E>,
        F: Environment,
        F::Dual: Environment,
        G: Environment,
        G::Dual: Environment,
    {
        if Arc::ptr_eq(&tx.rx.0, &rx.tx.0) {
            Ok(unsafe { Chan::with_env(tx.unwrap().0, rx.unwrap().1) })
        } else {
            Err(UnsplitError { tx, rx })
        }
    }
}

impl<Tx, Rx, E, P> Chan<Tx, Rx, P, E>
where
    P: Actionable<E>,
    E: Environment,
    E::Dual: Environment,
    <P::Env as EachSession>::Dual: Environment,
{
    /// Cast a channel to arbitrary new session types and environment. Use with care!
    unsafe fn cast<F, Q>(self) -> Chan<Tx, Rx, Q, F>
    where
        F: Environment,
        F::Dual: Environment,
        Q: Actionable<F>,
        <Q::Env as EachSession>::Dual: Environment,
    {
        let (tx, rx) = self.unwrap();
        Chan {
            tx,
            rx,
            environment: PhantomData,
            protocol: PhantomData,
        }
    }

    /// Unwrap a channel into its transmit and receive ends, exiting the regimen of session typing,
    /// potentially before the end of the session. **Prefer [`close`](Chan::close) to this method if you
    /// mean to unwrap a channel at the end of its session.**
    ///
    /// # Errors
    ///
    /// If this function is used before the end of a session, it may result in errors when the other
    /// end of the channel attempts to continue the session.
    ///
    /// # Examples
    ///
    /// ```
    /// use dialectic::*;
    ///
    /// let (c1, c2) = <Send<String>>::channel(backend::mpsc::unbounded_channel);
    /// let (tx1, rx1) = c1.unwrap();
    /// let (tx2, rx2) = c2.unwrap();
    /// ```
    pub fn unwrap(self) -> (Tx, Rx) {
        let tx = self.tx;
        let rx = self.rx;
        (tx, rx)
    }

    /// Create a new channel with an arbitrary environment. This is equivalent to casting a new
    /// channel to an arbitrary environment. Use with care!
    unsafe fn with_env(tx: Tx, rx: Rx) -> Chan<Tx, Rx, P, E> {
        Chan {
            tx,
            rx,
            environment: PhantomData,
            protocol: PhantomData,
        }
    }
}
