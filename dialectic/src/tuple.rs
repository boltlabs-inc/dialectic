//! Conversions back and forth between flat tuples and their corresponding inductive list
//! structures.
//!
//! Internally, this library uses inductive type-level lists, but presents an external interface in
//! terms of tuples, for readability. The traits here convert between the two equivalent
//! representations.
//!
//! At present, tuples up to size 128 are supported.

use super::unary::*;

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

/// Take the length of a type-level list as a unary type-level number.
pub trait HasLength {
    /// The length of a type-level list.
    type Length: Unary;
}

impl HasLength for () {
    type Length = Z;
}

impl<T, Ts: HasLength> HasLength for (T, Ts) {
    type Length = S<Ts::Length>;
}

dialectic_macro::impl_tuples!(256);
