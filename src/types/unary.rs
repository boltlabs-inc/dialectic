//! The unary numbers, represented by zero [`Z`] and successor [`S`].

pub mod constants;
pub mod types;

/// The number zero.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::Z;
///
/// let zero: Z = Z;
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Z;

/// The successor of `N` (i.e. `N + 1`).
///
/// # Examples
///
/// ```
/// use dialectic::prelude::{S, Z};
///
/// let one: S<Z> = S(Z);
/// ```
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct S<N>(pub N);

/// All unary numbers can be converted to their value-level equivalent `usize`.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
///
/// assert_eq!(_0::VALUE, 0);
/// assert_eq!(_1::VALUE, 1);
/// assert_eq!(_2::VALUE, 2);
/// // ...
/// assert_eq!(_127::VALUE, 127);
/// ```
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
///
/// # Examples
///
/// This compiles, because `1 < 2`:
///
/// ```
/// use dialectic::prelude::*;
///
/// fn ok() where _1: LessThan<_2> {}
/// ```
///
/// But this does not compile, because `2 >= 1`:
///
/// ```compile_fail
/// # use dialectic::prelude::*;
/// #
/// fn bad() where _2: LessThan<_1> {}
/// ```
///
/// Because [`LessThan`] is a *strict* less-than relationship (i.e. `<`, not `<=`), this does not
/// compile either:
///
/// ```compile_fail
/// # use dialectic::prelude::*;
/// #
/// fn bad() where _100: LessThan<_100> {}
/// ```
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
