//! Conversions back and forth between flat tuples like `(P, Q, R)` and their corresponding
//! inductive structures like `(P, (Q, (R, ())))`.
//!
//! Internally, this library uses inductive type-level lists, but presents an external interface in
//! terms of tuples, for readability. The traits here convert between the two equivalent
//! representations.
//!
//! At present, tuples up to size 128 are supported.

/// Convert a tuple into its corresponding inductive list structure.
pub trait Tuple: Sized {
    /// The corresponding inductive list.
    type AsList: List<AsTuple = Self>;
}

/// Convert an inductive list structure into its corresponding tuple.
pub trait List: Sized {
    /// The corresponding tuple.
    type AsTuple: Tuple<AsList = Self>;
}

mod impls;
pub use impls::*;
