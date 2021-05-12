//! `dialectic-reconnect` lets you wrap any Dialectic backend for automatic reconnection capability.
//! When a method on a [`Chan`] causes the underlying backend to throw some error, it will
//! automatically reconnect and retry using a [`backoff::Backoff`] strategy, propagating the error
//! to the caller only if all retry attempts fail.
//!
//! # Wrapping a backend
//!
//! To illustrate these concepts, we'll wrap a `PingPong` protocol over a `dialectic_tokio_serde`
//! backend:
//!
//! ```rust
//! use dialectic::prelude::*;
//! use dialectic_tokio_serde::codec::LinesCodec;
//! use dialectic_tokio_serde_json::{self as json, Json};
//! use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
//!
//! pub type PingPong = Session! {
//!     loop {
//!         send ();
//!         recv ();
//!     }
//! };
//!
//! pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
//! pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;
//! ```
//!
//! ## Defining a handshake
//!
//! A *handshake* is a protocol that takes place when intializing the [`Chan`] or retrying a method
//! after a failure. In our example protocol, we'll ask for a session key when connecting and
//! present this session key each time we retry.
//!
//! ```rust
//! use dialectic::prelude::*;
//!
//! pub type Handshake = Session! {
//!     choose {
//!         // Start a new session: receive a session key from the server
//!         0 => recv usize,
//!         // Resume an existing session: submit a session key to the server
//!         1 => send usize,
//!     }
//! };
//! ```
//!
//! ## Retrying and resuming
//!
//! Next, we assign each side of the [`Chan`] the dual roles of the [`retry`]-ing end and the
//! [`resume`]-ing end.
//!
//! ## The [`retry`]-ing end
//!
//! The [`retry`]-ing end knows how to make the initial connection and will actively try to
//! re-establish the connection when there's an error. In our `PingPong` protocol, this will be the
//! pinging end. We'll first need to define some helpers.
//!
//! ```no_run
//! # use dialectic::prelude::*;
//! # use dialectic_tokio_serde::codec::LinesCodec;
//! # use dialectic_tokio_serde_json::{self as json, Json};
//! # use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
//! #
//! # pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
//! # pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;
//! #
//! # pub type Handshake = Session! {
//! #    choose {
//! #        // Start a new session: receive a session key from the server
//! #        0 => recv usize,
//! #        // Resume an existing session: submit a session key to the server
//! #        1 => send usize,
//! #    }
//! # };
//! use std::net::IpAddr;
//! use anyhow::Error;
//! use tokio::net::TcpStream;
//!
//! // How to connect to an address and generate a `Tx`/`Rx` pair
//! async fn connect(addr: (IpAddr, u16)) -> Result<(Tx, Rx), Error> {
//!     let (rx, tx) = TcpStream::connect(addr).await?.into_split();
//!     Ok(json::lines(tx, rx, 1024 * 8))
//! }
//!
//! // How to perform the initial handshake, receiving a session key from the server
//! async fn init(chan: Chan<Handshake, Tx, Rx>) -> Result<usize, Error> {
//!     Ok(chan.choose::<0>().await?.recv().await?.0)
//! }
//!
//! // How to perform a retry handshake, submitting a session key to the server
//! async fn retry(key: usize, chan: Chan<Handshake, Tx, Rx>) -> Result<(), Error> {
//!     let chan = chan.choose::<1>().await?;
//!     chan.send(key).await?.close();
//!     Ok(())
//! }
//! ```
//!
//! ### Defining a [`retry::Connector`]
//!
//! We need to tell the [`retry::Connector`] how to behave if there's an error in different
//! circumstances. To do this, we use [`recover_connect`][], [`recover_handshake`][],
//! [`recover_tx`][], and [`recover_rx`][], which each accept a Rust closure.
//!
//! [`recover_handshake`]: retry::Connector::recover_handshake
//!
//! [`recover_connect`]: retry::Connector::recover_connect
//!
//! [`recover_tx`]: retry::Connector::recover_tx
//!
//! [`recover_rx`]: retry::Connector::recover_rx
//!
//! Putting it all together:
//!
//! ```no_run
//! # use dialectic::prelude::*;
//! # use dialectic_tokio_serde::codec::LinesCodec;
//! # use dialectic_tokio_serde_json::{self as json, Json};
//! # use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
//! # use std::net::IpAddr;
//! # use anyhow::Error;
//! # use tokio::net::TcpStream;
//! #
//! # pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
//! # pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;
//! #
//! # pub type PingPong = Session! {
//! #     loop {
//! #         send ();
//! #         recv ();
//! #     }
//! # };
//! #
//! # pub type Handshake = Session! {
//! #    choose {
//! #        // Start a new session: receive a session key from the server
//! #        0 => recv usize,
//! #        // Resume an existing session: submit a session key to the server
//! #        1 => send usize,
//! #    }
//! # };
//! # // How to connect to an address and generate a `Tx`/`Rx` pair
//! # async fn connect(addr: (IpAddr, u16)) -> Result<(Tx, Rx), Error> {
//! #     let (rx, tx) = TcpStream::connect(addr).await?.into_split();
//! #     Ok(json::lines(tx, rx, 1024 * 8))
//! # }
//! #
//! # // How to perform the initial handshake, receiving a session key from the server
//! # async fn init(chan: Chan<Handshake, Tx, Rx>) -> Result<usize, Error> {
//! #     Ok(chan.choose::<0>().await?.recv().await?.0)
//! # }
//! #
//! # // How to perform a retry handshake, submitting a session key to the server
//! # async fn retry(key: usize, chan: Chan<Handshake, Tx, Rx>) -> Result<(), Error> {
//! #     let chan = chan.choose::<1>().await?;
//! #     chan.send(key).await?.close();
//! #    Ok(())
//! # }
//! use std::{convert::TryFrom, time::Duration};
//! use tokio::time::sleep;
//! use dialectic_reconnect::{Backoff, retry::{self, Connector}};
//!
//! // This is a quick way to generate closures that implement various retry strategies.
//! fn backoff<E>(retries: usize, error: &E) -> retry::Recovery {
//!     Backoff::with_delay(Duration::from_millis(100))
//!         .exponential(2.0)
//!         .jitter(Duration::from_millis(10))
//!         .max_delay(Duration::from_secs(1))
//!         .build(retry::Recovery::ReconnectAfter)(retries, error)
//! }
//!
//! # #[tokio::main]
//! # async fn main() -> Result<(), anyhow::Error> {
//! // Connect, using our protocol, attempting to recover using the backoff strategy for each error
//! let (key, mut chan) = Connector::new(connect, init, retry, PingPong::default())
//!     .recover_connect(backoff)
//!     .recover_handshake(backoff)
//!     .recover_tx(backoff)
//!     .recover_rx(backoff)
//!     .connect((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000))
//!     .await?;
//!
//! // Run the ping protocol forever
//! loop {
//!     chan = chan.send(()).await?.recv().await?.1;
//!     sleep(Duration::from_millis(100)).await;
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ## The [`resume`]-ing end
//!
//! The [`resume`]-ing end accepts incoming connections and performs the other side of the handshake
//! protocol. The [`resume`]-ing end's handshake session type must be the dual of the corresponding
//! [`retry::Connector`]'s to work together harmoniously. In our `PingPong` example, it's the side
//! receiving the ping.
//!
//! First, we define a helper to decide whether an incoming connection is a new session or if
//! they're resuming an existing session and return a session key accordingly:
//!
//! ```no_run
//! # use dialectic::prelude::*;
//! # use dialectic_tokio_serde::codec::LinesCodec;
//! # use dialectic_tokio_serde_json::{self as json, Json};
//! # use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
//! # use anyhow::Error;
//! #
//! # pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
//! # pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;
//! #
//! # pub type PingPong = Session! {
//! #     loop {
//! #         send ();
//! #         recv ();
//! #     }
//! # };
//! #
//! # pub type Handshake = Session! {
//! #    choose {
//! #        // Start a new session: receive a session key from the server
//! #        0 => recv usize,
//! #        // Resume an existing session: submit a session key to the server
//! #        1 => send usize,
//! #    }
//! # };
//! use dialectic_reconnect::resume::ResumeKind;
//! use std::sync::{atomic::{Ordering, AtomicUsize}, Arc};
//!
//! // We assign keys to sessions based on sequential ordering
//! let key = Arc::new(AtomicUsize::new(0));
//!
//! // The handshake either generates a new key and sends it, or receives an existing key
//! let handshake = move |chan: Chan<<Handshake as Session>::Dual, Tx, Rx>| {
//!     let key = key.clone();
//!     async move {
//!         let output = offer!(in chan {
//!             0 => {
//!                 // Client wants to start a new session
//!                 let new_key = key.fetch_add(1, Ordering::SeqCst);
//!                 chan.send(new_key).await?.close();
//!                 (ResumeKind::New, new_key)
//!             },
//!             1 => {
//!                 // Client wants to resume an existing session
//!                 let (existing_key, chan) = chan.recv().await?;
//!                 chan.close();
//!                 (ResumeKind::Existing, existing_key)
//!             }
//!         })?;
//!         Ok::<_, Error>(output)
//!     }
//! };
//! ```
//!
//! ### Defining a [`resume::Acceptor`]
//!
//! Similar to the [`retry::Connector`], we can provide closures to handle transmitter and receiver
//! errors with [`recover_tx`][] and [`recover_rx`][]. This time, we'll try to reconnect immediately
//! without any backoff.
//!
//! [`recover_tx`]: resume::Acceptor::recover_tx [`recover_rx`]: resume::Acceptor::recover_rx
//!
//! ```no_run
//! # use dialectic::prelude::*;
//! # use dialectic_tokio_serde::codec::LinesCodec;
//! # use dialectic_tokio_serde_json::{self as json, Json};
//! # use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
//! # use anyhow::Error;
//! # use std::net::IpAddr;
//! #
//! # pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
//! # pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;
//! #
//! # pub type PingPong = Session! {
//! #     loop {
//! #         send ();
//! #         recv ();
//! #     }
//! # };
//! #
//! # pub type Handshake = Session! {
//! #    choose {
//! #        // Start a new session: receive a session key from the server
//! #        0 => recv usize,
//! #        // Resume an existing session: submit a session key to the server
//! #        1 => send usize,
//! #    }
//! # };
//! # use dialectic_reconnect::resume::ResumeKind;
//! # use std::{convert::TryFrom, time::Duration};
//! # use std::sync::{atomic::{Ordering, AtomicUsize}, Arc};
//! #
//! # #[tokio::main]
//! # async fn main() -> Result<(), anyhow::Error> {
//! # // We assign keys to sessions based on sequential ordering
//! # let key = Arc::new(AtomicUsize::new(0));
//! #
//! # // The handshake either generates a new key and sends it, or receives an existing key
//! # let handshake = move |chan: Chan<<Handshake as Session>::Dual, Tx, Rx>| {
//! #     let key = key.clone();
//! #     async move {
//! #         let output = offer!(in chan {
//! #             0 => {
//! #                 // Client wants to start a new session
//! #                 let new_key = key.fetch_add(1, Ordering::SeqCst);
//! #                 chan.send(new_key).await?.close();
//! #                 (ResumeKind::New, new_key)
//! #             },
//! #             1 => {
//! #                 // Client wants to resume an existing session
//! #                 let (existing_key, chan) = chan.recv().await?;
//! #                 chan.close();
//! #                 (ResumeKind::Existing, existing_key)
//! #             }
//! #         })?;
//! #         Ok::<_, Error>(output)
//! #     }
//! # };
//! #
//! use tokio::net::TcpListener;
//! use dialectic_reconnect::resume::Acceptor;
//! use dialectic_tokio_serde_json as json;
//!
//! // Create an acceptor for our handshake and the dual of the protocol
//! let acceptor = Acceptor::new(handshake, <<PingPong as Session>::Dual>::default());
//!
//! // Accept connections, running the dual of the ping protocol on each, forever
//! let listener = TcpListener::bind((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000)).await?;
//!
//! loop {
//!     // Accept a TCP connection and wrap it in a json-lines serialization layer
//!     let (tcp_stream, _) = listener.accept().await?;
//!     let (rx, tx) = tcp_stream.into_split();
//!     let (tx, rx) = json::lines(tx, rx, 1024 * 8);
//!
//!     // Accept these `tx`/`rx` ends; if they generate a new channel, run the pong-ping protocol
//!     if let (key, Some(mut chan)) = acceptor.accept(tx, rx).await? {
//!         tokio::spawn(async move {
//!             loop {
//!                 chan = chan.recv().await?.1.send(()).await?;
//!             }
//!             Ok::<_, Error>(())
//!         });
//!     }
//! }
//! # Ok(())
//! # }
//! ```
//!
//! To view the full working example of the `PingPong` protocol, see [the examples directory in the
//! dialectic-reconnect crate][examples].
//!
//! [`Chan`]: dialectic::Chan
//!
//! [examples]: https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-reconnect/examples
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
#![forbid(broken_intra_doc_links)]
mod backoff;
mod maybe_bounded;
pub mod resume;
pub mod retry;
mod util;
pub use backoff::Backoff;
