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
pub trait Unary: sealed::Unary + Sized + Sync + Send + 'static {
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
pub trait LessThan<N: sealed::Unary>
where
    Self: sealed::Unary,
{
}

impl<N: sealed::Unary> LessThan<S<N>> for Z {}

impl<N: sealed::Unary, M: LessThan<N>> LessThan<S<N>> for S<M> {}

/// Compare two unary numbers and branch on their comparison, at the type level.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use static_assertions::assert_type_eq_all;
///
/// assert_type_eq_all!(<(_0, _1) as Compare<u8, u16, u32>>::Result, u8);
/// assert_type_eq_all!(<(_1, _1) as Compare<u8, u16, u32>>::Result, u16);
/// assert_type_eq_all!(<(_2, _1) as Compare<u8, u16, u32>>::Result, u32);
/// ```
pub trait Compare<IfLess, IfEqual, IfGreater>: sealed::Compare {
    /// The result of the comparison: either `T` if `Self == N` or `E` if `Self != N`.
    type Result;
}

impl<N: sealed::Unary, M: sealed::Unary, IfLess, IfEqual, IfGreater>
    Compare<IfLess, IfEqual, IfGreater> for (S<N>, S<M>)
where
    (N, M): Compare<IfLess, IfEqual, IfGreater>,
{
    type Result = <(N, M) as Compare<IfLess, IfEqual, IfGreater>>::Result;
}

impl<IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater> for (Z, Z) {
    type Result = IfEqual;
}

impl<N: sealed::Unary, IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater>
    for (S<N>, Z)
{
    type Result = IfGreater;
}

impl<N: sealed::Unary, IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater>
    for (Z, S<N>)
{
    type Result = IfLess;
}

mod sealed {
    use super::*;
    pub trait Unary: 'static {}
    impl Unary for Z {}
    impl<N: Unary> Unary for S<N> {}

    pub trait Compare {}
    impl<N: Unary, M: Unary> Compare for (N, M) {}
}
