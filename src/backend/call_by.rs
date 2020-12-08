//! To parameterize a function by calling convention, we can specify that it takes some `T:
//! CallBy<'a, Convention>`, and say that its input is of type `<T as CallBy<'a,
//! Convention>>::Type`. This is essentially a defunctionalization of Rust's reference operators.
//!
//! This trick can be used to permit the *implementor* of a trait to pick the calling convention for
//! a value passed into (or out of) a function defined in that trait, rather than this being
//! hardcoded in the trait definition.
//!
//! # Examples
//!
//! For instance, say we wanted to define an abstraction for channels that can send values. Imagine,
//! however, that some channels might need to take ownership of the values they send, while others
//! might serialize values given only a reference to that value. In order to unify these two notions
//! into one trait, we can parameterize over the calling convention for the input value:
//!
//! ```
//! use dialectic::{CallBy, CallingConvention};
//!
//! trait Sender<'a, T>
//! where
//!     T: CallBy<'a, Self::Convention>,
//! {
//!     type Convention: CallingConvention;
//!     fn send(&self, value: <T as CallBy<'a, Self::Convention>>::Type);
//! }
//! ```
//!
//! Implementers of the `Sender` trait can choose whether the associated type `Convention` should be
//! [`Val`], [`Ref`], or [`Mut`], which toggles the result of `<T as CallBy<'a,
//! Self::Convention>>::Type` between `T`, `&'a T`, and `&'a mut T`, respectively. Meanwhile,
//! callers of the `send` method on concretely known types don't need to specify the calling
//! convention; the type-level function determines what type they need to pass as the argument to
//! `send`, and type errors are reported in reference to that concrete type if it is known at the
//! call site.

/// There are three fundamental ways to pass a `T` as input or return a `T` as output: by [`Val`]ue,
/// by shared immutable [`Ref`]erence, and by unique [`Mut`]able reference.
///
/// This is a sealed trait, implemented for all three of these conventions.
pub trait CallingConvention: sealed::CallingConvention {}
impl CallingConvention for Val {}
impl CallingConvention for Ref {}
impl CallingConvention for Mut {}

/// To get the type of `T` via calling convention `Convention`, write `<T as CallBy<'a,
/// Convention>>::Type`.
pub trait CallBy<'a, Convention: CallingConvention> {
    /// The type of `Self` when called by `Convention`.
    type Type;
}

/// Taking a `T` by [`Val`]ue means taking a `T` as input to or output from a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Val;

impl<'a, T> CallBy<'a, Val> for T {
    type Type = T;
}

/// Taking a `T` by [`Ref`]erence means taking `&'a T` as input to or output from a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Ref;

impl<'a, T: 'a> CallBy<'a, Ref> for T {
    type Type = &'a T;
}

/// Taking a `T` by [`Mut`]able reference means taking `&'a mut T` as input to or output from a
/// function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Mut;

impl<'a, T: 'a> CallBy<'a, Mut> for T {
    type Type = &'a mut T;
}

mod sealed {
    use super::*;

    pub trait CallingConvention {}
    impl CallingConvention for Val {}
    impl CallingConvention for Ref {}
    impl CallingConvention for Mut {}
}
