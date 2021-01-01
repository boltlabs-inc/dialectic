//! Available formats for serialization, and helper functions for common pairings of
//! serialization formats and framing codecs.
//!
//! By default, none of the formats are enabled. To enable a format, use its corresponding
//! feature when listing this crate in your `Cargo.toml`. For instance, to enable JSON support:
//!
//! ```toml
//! [dependencies]
//! dialectic = { version = "...", features = ["json"] }
//! ```
//!
//! The currently supported formats are:
//!
//! - `"json"`, via the [`serde_json`] crate
//! - `"bincode"`, via the [`bincode`](bincode_crate) crate

#[cfg(feature = "bincode")]
pub use bincode::*;

#[cfg(feature = "bincode")]
mod bincode;

#[cfg(feature = "json")]
pub use json::*;

#[cfg(feature = "json")]
mod json;
