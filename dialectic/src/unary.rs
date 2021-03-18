//! The unary numbers, represented by zero [`Z`] and successor [`S`].

/// The number zero.
///
/// # Examples
///
/// ```
/// use dialectic::unary::Z;
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
/// use dialectic::unary::{S, Z};
///
/// let one: S<Z> = S(Z);
/// ```
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct S<N>(pub N);

/// A convenient type synonym for writing out unary types using constants.
pub type UnaryOf<const N: usize> = <Number<N> as ToUnary>::AsUnary;

/// All unary numbers can be converted to their value-level equivalent `usize`.
///
/// # Examples
///
/// ```
/// # #![recursion_limit = "256"]
/// use dialectic::prelude::*;
/// use dialectic::unary::*;
///
/// assert_eq!(<UnaryOf<0>>::VALUE, 0);
/// assert_eq!(<UnaryOf<1>>::VALUE, 1);
/// assert_eq!(<UnaryOf<2>>::VALUE, 2);
/// // ...
/// assert_eq!(<UnaryOf<256>>::VALUE, 256);
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
/// use dialectic::unary::*;
///
/// fn ok() where UnaryOf<1>: LessThan<UnaryOf<2>> {}
/// ```
///
/// But this does not compile, because `2 >= 1`:
///
/// ```compile_fail
/// # use dialectic::prelude::*;
/// # use dialectic::unary::*;
/// #
/// fn bad() where UnaryOf<2>: LessThan<UnaryOf<1>> {}
/// ```
///
/// Because [`LessThan`] is a *strict* less-than relationship (i.e. `<`, not `<=`), this does not
/// compile either:
///
/// ```compile_fail
/// # use dialectic::prelude::*;
/// # use dialectic::unary::*;
/// #
/// fn bad() where UnaryOf<100>: LessThan<UnaryOf<100>> {}
/// ```
pub trait LessThan<N: Unary>
where
    Self: Unary,
{
}

impl<N: Unary> LessThan<S<N>> for Z {}

impl<N: Unary, M: LessThan<N>> LessThan<S<N>> for S<M> {}

/// Compare two unary numbers and branch on their comparison, at the type level.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::unary::{Compare, UnaryOf};
/// use static_assertions::assert_type_eq_all;
///
/// assert_type_eq_all!(<(UnaryOf<0>, UnaryOf<1>) as Compare<u8, u16, u32>>::Result, u8);
/// assert_type_eq_all!(<(UnaryOf<1>, UnaryOf<1>) as Compare<u8, u16, u32>>::Result, u16);
/// assert_type_eq_all!(<(UnaryOf<2>, UnaryOf<1>) as Compare<u8, u16, u32>>::Result, u32);
/// ```
pub trait Compare<IfLess, IfEqual, IfGreater>: sealed::Compare {
    /// The result of the comparison: either `T` if `Self == N` or `E` if `Self != N`.
    type Result;
}

impl<N: Unary, M: Unary, IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater>
    for (S<N>, S<M>)
where
    (N, M): Compare<IfLess, IfEqual, IfGreater>,
{
    type Result = <(N, M) as Compare<IfLess, IfEqual, IfGreater>>::Result;
}

impl<IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater> for (Z, Z) {
    type Result = IfEqual;
}

impl<N: Unary, IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater> for (S<N>, Z) {
    type Result = IfGreater;
}

impl<N: Unary, IfLess, IfEqual, IfGreater> Compare<IfLess, IfEqual, IfGreater> for (Z, S<N>) {
    type Result = IfLess;
}

/// Add two unary numbers at the type level.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::unary::*;
/// use static_assertions::assert_type_eq_all;
///
/// assert_type_eq_all!(<(UnaryOf<1>, UnaryOf<1>) as Add>::Result, UnaryOf<2>);
/// assert_type_eq_all!(<(UnaryOf<5>, UnaryOf<7>) as Add>::Result, UnaryOf<12>);
/// ```
pub trait Add: sealed::Add {
    /// The result of the addition.
    type Result: Unary;
}

impl<N: Unary> Add for (N, Z) {
    type Result = N;
}

impl<N: Unary, M: Unary> Add for (N, S<M>)
where
    (N, M): Add,
{
    type Result = S<<(N, M) as Add>::Result>;
}

/// A trait marking wrapped type-level constants.
pub trait Constant: sealed::Constant {}

/// A wrapper for type-level `usize` values to allow implementing traits on them.
#[allow(missing_debug_implementations)]
pub struct Number<const N: usize>;

impl<const N: usize> Constant for Number<N> {}

/// A trait which allows conversion from a wrapper type over a type-level `usize` to a unary
/// type-level number representation.
pub trait ToUnary {
    /// The result of conversion.
    type AsUnary: Unary + ToConstant<AsConstant = Self>;
}

/// A trait which allows conversion from a unary type-level representation to a wrapper over a
/// type-level `usize`.
pub trait ToConstant: Unary {
    /// The result of conversion.
    type AsConstant: Constant + ToUnary<AsUnary = Self>;
}

dialectic_macro::generate_unary_conversion_impls!(256);

mod sealed {
    use super::*;
    pub trait Unary: 'static {}
    impl Unary for Z {}
    impl<N: Unary> Unary for S<N> {}

    pub trait Constant: 'static {}
    impl<const N: usize> Constant for Number<N> {}

    pub trait Compare {}
    impl<N: Unary, M: Unary> Compare for (N, M) {}

    pub trait Add {}
    impl<N: Unary, M: Unary> Add for (N, M) {}
}
