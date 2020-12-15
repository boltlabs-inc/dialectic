//! The unary numbers, represented by zero [`Z`] and successor [`S`].

pub mod constants;
pub mod types;

/// The number zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Z;

/// The successor of `N` (i.e. `N + 1`).
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct S<N>(pub N);

/// All unary numbers can be converted to their value-level equivalent `usize`.
pub trait Unary: sealed::Sealed {
    /// The runtime value of this type-level number, as a `usize`.
    const VALUE: usize;
}

impl Unary for Z {
    const VALUE: usize = 0;
}

impl<N: Unary> Unary for S<N> {
    const VALUE: usize = N::VALUE + 1;
}

/// Ensure than a unary number is strictly less than some other number.
pub trait LessThan<N: sealed::Sealed>
where
    Self: sealed::Sealed,
{
}

impl<N: sealed::Sealed> LessThan<S<N>> for Z {}

impl<N: sealed::Sealed, M: LessThan<N>> LessThan<S<N>> for S<M> {}

mod sealed {
    use super::*;
    pub trait Sealed {}
    impl Sealed for Z {}
    impl<N: Sealed> Sealed for S<N> {}
}
