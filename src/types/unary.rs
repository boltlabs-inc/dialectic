//! The unary numbers, represented by zero [`Z`] and successor [`S`].

/// The number zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Z;

/// The successor of `N` (i.e. `N + 1`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct S<N>(N);

/// All unary numbers can be converted to their value-level equivalent `usize`.
pub trait Unary: sealed::Sealed {
    const VALUE: usize;
}

impl Unary for Z {
    const VALUE: usize = 0;
}

impl<N: Unary> Unary for S<N> {
    const VALUE: usize = N::VALUE + 1;
}

mod sealed {
    use super::*;
    pub trait Sealed {}
    impl Sealed for Z {}
    impl<N: Sealed> Sealed for S<N> {}
}
