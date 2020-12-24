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

#[cfg(feature = "bincode")]
pub use bincode::*;

#[cfg(feature = "bincode")]
mod bincode;

#[cfg(feature = "json")]
pub use json::*;

#[cfg(feature = "json")]
mod json;
