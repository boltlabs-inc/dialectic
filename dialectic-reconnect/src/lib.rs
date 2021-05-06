//! `dialectic-reconnect` lets you wrap any Dialectic backend for automatic reconnection
//! capability. When a method on a [`Chan`] causes the underlying backend to throw some error, it
//! will automatically reconnect and retry using a [`backoff::Backoff`] strategy, propagating the
//! error to the caller only if all retry attempts fail.
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
//! # use dialectic::prelude::*;
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
//! [`recover_connect`]: retry::Connector::recover_connect
//! [`recover_tx`]: retry::Connector::recover_tx
//! [`recover_rx`]: retry::Connector::recover_rx
//!
//! Putting it all together:
//!
//! ```no_run
//! // This is a quick way to generate closures that implement various retry strategies.
//! let backoff = Backoff::with_delay(Duration::from_millis(100))
//!     .exponential(2.0)
//!     .jitter(Duration::from_millis(10))
//!     .max_delay(Duration::from_secs(1));
//!
//! // A connector for our protocol, with a 10 second timeout, which attempts to recover using
//! // the backoff strategy for every kind of error
//! let connector = Connector::new(connect, init, retry)
//!     .session::<PingPong>()
//!     .timeout(Duration::from_secs(10))
//!     .recover_connect(backoff.build(ReconnectStrategy::ReconnectAfter))
//!     .recover_handshake(backoff.build(ReconnectStrategy::ReconnectAfter))
//!     .recover_tx(backoff.build(RetryStrategy::ReconnectAfter))
//!     .recover_rx(backoff.build(RetryStrategy::ReconnectAfter));
//! ```
//!
//! ## The [`resume`]-ing end
//!
//! The [`resume`]-ing end accepts incoming connections and performs the other side of the
//! handshake protocol. The [`resume`]-ing end's handshake session type must be the dual of the
//! corresponding [`retry::Connector`]'s to work together harmoniously. In our `PingPong` example,
//! it's the side receiving the ping.
//!
//! First, we define a helper to decide whether an incoming connection is a new session or if
//! they're resuming an existing session and return a session key accordingly:
//!
//! ```no_run
//! // We assign keys to sessions based on sequential ordering
//! let key = Arc::new(AtomicUsize::new(0));
//!
//! // The handshake either generates a new key and sends it, or receives an existing key
//! let handshake = move |chan: Chan<Handshake, Tx, Rx>| {
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
//! errors with [`recover_tx`][] and [`recover_rx`][]. This time, we'll try to reconnect
//! immediately without any backoff, but with some logging.
//!
//! [`recover_tx`]: resume::Acceptor::recover_tx
//! [`recover_rx`]: resume::Acceptor::recover_rx
//!
//! ```no_run
//! type PongPing = <ping::PingPong as Session>::Dual;
//! // Create an acceptor for our protocol which times out at 10 seconds and logs errors
//! let acceptor = Acceptor::new(handshake)
//!     .session::<PongPing>()
//!     .buffer_size(10)
//!     .timeout(Duration::from_secs(10))
//!     .recover_tx({
//!         let log = log.clone();
//!         move |retries, error| {
//!             let _ = log.send(format!("[TX error] retries: {}, error: {}", retries, error));
//!             ResumeStrategy::Reconnect
//!         }
//!     })
//!     .recover_rx({
//!         let log = log.clone();
//!         move |retries, error| {
//!             let _ = log.send(format!("[RX error] retries: {}, error: {}", retries, error));
//!             ResumeStrategy::Reconnect
//!         }
//!     });
//! ```
//!
//! To view the full working example of the `PingPong` protocol, see [the examples directory in the
//! dialectic-reconnect crate](examples).
//!
//! [`Chan`]: dialectic::Chan
//! [examples]: https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-reconnect/examples
mod backoff;
pub mod resume;
pub mod retry;
mod util;
pub use backoff::Backoff;