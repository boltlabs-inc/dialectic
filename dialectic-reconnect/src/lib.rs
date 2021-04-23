//! This crate provides a wrapper for any Dialectic backend which augments it with automatic
//! reconnection capability, so that when a method on a [`Chan`] causes the underlying backend to
//! throw some error, it is only propagated to the caller if it is not possible to reconnect and
//! resume the connection.
//!
//! The communicating parties are assigned the dual roles of the [`retry`]-ing end, which actively
//! attempts to re-establish the connection, and the [`resume`]-ing end, which passively awaits its
//! re-establishment. To construct an application which makes use of this functionality, one end
//! must use a [`retry::Connector`] to create retry-enabled [`Chan`]s, and the other end must use a
//! [`resume::Acceptor`] to create resume-enabled [`Chan`]s.
//!
//! Each side of the connection can be parameterized by arbitrary *reconnection strategies*, defined
//! as Rust closures, which describe under what circumstances and in what manner a given end of the
//! connection should attempt to reconnect after a particular error.
//!
//! When the retrying end attempts to reconnect to the resuming end, it performs a *handshake* to
//! notify the resuming end whether it is a [`New`](resume::ResumeKind::New) connection or
//! attempting to resume an [`Existing`](resume::ResumeKind::Existing) connection. If the connection
//! is a new connection, the retrying end and resuming end will also agree on a mutually known
//! *session key* to identify the session in the future. The communication protocol to perform the
//! handshake and the type of the session key it yields are parameters of both the retrying and
//! resuming end. In order for an [`Acceptor`](resume::Acceptor) and a
//! [`Connector`](retry::Connector) to work together harmoniously, their handshake session types
//! must be dual.
//!
//! [`Chan`]: dialectic::Chan
mod backoff;
pub mod resume;
pub mod retry;
mod util;
pub use backoff::Backoff;
