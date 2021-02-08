//! The types in this module enumerate the shapes of all expressible sessions.

use crate::prelude::*;
use crate::unary::{LessThan, Unary, S, Z};

// Each construct in the session types language lives in its own module, along with the
// implementation of its various typing rules.
mod choose;
mod r#continue;
mod done;
mod r#loop;
mod offer;
mod recv;
mod send;
mod seq;
mod split;

pub use choose::*;
pub use done::*;
pub use offer::*;
pub use r#continue::*;
pub use r#loop::*;
pub use recv::*;
pub use send::*;
pub use seq::*;
pub use split::*;

/// Each session type has a [`HasDual::DualSession`], the type of the corresponding client on the
/// other side of the channel. The sealed trait `HasDual` enumerates these types, and provides the
/// dual of each.
///
/// 💡 In general, you should prefer the [`Session`] trait to the [`HasDual`] trait, since
/// [`Session`] also ensures that a given type is a valid session type and provides other
/// functionality.
///
/// # Examples
///
/// Here we define a `Client` and `Server` session type, which are duals of each other. This example
/// illustrates every construct in the language of session types.
///
/// ```
/// # use static_assertions::assert_type_eq_all;
/// use dialectic::types::*;
///
/// type Client = Loop<Offer<(Split<Send<String, Done>, Recv<usize, Done>>, Recv<bool, Continue>)>>;
/// type Server = Loop<Choose<(Split<Send<usize, Done>, Recv<String, Done>>, Send<bool, Continue>)>>;
///
/// assert_type_eq_all!(Client, <Server as HasDual>::DualSession);
/// ```
pub trait HasDual: sealed::IsSession + Sized + 'static {
    /// The dual to this session type, i.e. the session type required of the other end of the
    /// channel.
    type DualSession: HasDual<DualSession = Self>;
}

/// Each session type has a canonical [`Actionable::NextAction`], the session type which corresponds
/// to the next thing to do on the channel. For most types, this is the same as `Self`, but for
/// control constructs like [`Loop`], this corresponds to the inside of the [`Loop`].
///
/// 💡 In general, you should prefer the [`Session`] trait to the [`Actionable`] trait, since
/// [`Session`] also ensures that a given type is a valid session type and provides other
/// functionality.
pub trait Actionable: sealed::IsSession {
    /// The next actual channel action, which must be one of [`Send`], [`Recv`], [`Offer`],
    /// [`Choose`], [`Split`], [`Seq`], or [`Done`].
    ///
    /// The constraints on this associated type ensure that it is idemopotent: the `Action` and of
    /// an `Action` is the same as that `Action`.
    type NextAction: Actionable<NextAction = Self::NextAction>;
}

/// A session type is [`Scoped`] if none of its [`Continue`]s refer to outside of the
/// [`Loop`]s which they are within.
///
/// 💡 In general, you should prefer the [`Session`] trait to the [`Scoped`] trait, since
/// [`Session`] also ensures that a given type is a valid session type and provides other
/// functionality.
pub trait Scoped<N: Unary = Z>: sealed::IsSession {}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachScoped`] trait ensures that every protocol in a type level list of
/// protocols is [`Scoped`].
pub trait EachScoped<N: Unary = Z>: sealed::EachSession {}
impl<N: Unary> EachScoped<N> for () {}
impl<N: Unary, P: Scoped<N>, Ps: EachScoped<N>> EachScoped<N> for (P, Ps) {}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachHasDual`] trait ensures that every protocol in a type level list of
/// protocols [`HasDual`].
pub trait EachHasDual: sealed::EachSession + Sized + 'static
where
    Self::Duals: EachHasDual<Duals = Self>,
{
    /// The point-wise [`HasDual::DualSession`] of a type-level list of session types.
    type Duals;
}

impl EachHasDual for () {
    type Duals = ();
}

impl<P, Ps> EachHasDual for (P, Ps)
where
    P: HasDual,
    Ps: EachHasDual,
{
    type Duals = (P::DualSession, Ps::Duals);
}

/// Substitute `P` for every [`Continue`] referring to the outermost scope in the given session
/// type.
///
/// When entering a [`Loop`], all [`Continue`]s which refer to that [`Loop`] are unrolled by one
/// loop iteration so that the session type remains valid. This trait implements that type-level
/// operation.
///
/// # Examples
///
/// ```
/// use dialectic::types::*;
/// use dialectic::prelude::*;
/// # use static_assertions::assert_type_eq_all;
///
/// assert_type_eq_all!(
///     <Send<i64, Continue> as Subst<Recv<(), Done>>>::Substituted,
///     Send<i64, Recv<(), Done>>,
/// );
/// ```
pub trait Subst<P, N: Unary = Z>: sealed::IsSession {
    /// The result of the substitution.
    type Substituted: 'static;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSubst`] trait ensures that every protocol in a type level list of
/// protocols can [`Subst`].
pub trait EachSubst<P, N: Unary = Z>: sealed::EachSession {
    /// The result of the substitution on every element of the list.
    type Substituted: 'static;
}

impl<N: Unary, Q> EachSubst<Q, N> for () {
    type Substituted = ();
}

impl<N: Unary, Q, P, Ps> EachSubst<Q, N> for (P, Ps)
where
    P: Subst<Q, N>,
    Ps: EachSubst<Q, N>,
{
    type Substituted = (P::Substituted, Ps::Substituted);
}

/// Select by index from a type level list.
///
/// This is used internally by the [`Choose`], [`Offer`], and [`Continue`] session types.
///
/// # Examples
///
/// ```
/// # use static_assertions::assert_type_eq_all;
/// use dialectic::types::*;
/// use dialectic::prelude::*;
///
/// assert_type_eq_all!(<(_0, (_1, (_2, ()))) as Select<_0>>::Selected, _0);
/// assert_type_eq_all!(<(_0, (_1, (_2, ()))) as Select<_2>>::Selected, _2);
/// ```
pub trait Select<N: Unary>: sealed::Select<N> {
    /// The thing which is selected from this list by the index `N`.
    type Selected;

    /// The rest of the list after the selected thing.
    type Remainder;
}

impl<T, Rest> Select<Z> for (T, Rest) {
    type Selected = T;
    type Remainder = Rest;
}

impl<T, P, Rest, N: Unary> Select<S<N>> for (T, (P, Rest))
where
    (P, Rest): Select<N>,
{
    type Selected = <(P, Rest) as Select<N>>::Selected;
    type Remainder = <(P, Rest) as Select<N>>::Remainder;
}

mod sealed {
    use std::any::Any;

    use super::*;

    /// Seal the [`Session`] trait so only types defined in this crate can be session types.
    pub trait IsSession: Any {}

    /// Seal the [`EachSession`] trait so it can't be extended in weird ways.
    pub trait EachSession {}
    impl EachSession for () {}
    impl<T: IsSession, Ts: EachSession> EachSession for (T, Ts) {}

    /// Seal the [`Select`] trait so it can't be extended in weird ways.
    pub trait Select<N: Unary> {}
    impl<T, S> Select<Z> for (T, S) {}
    impl<T, P, Rest, N: Unary> Select<S<N>> for (T, (P, Rest)) where (P, Rest): Select<N> {}
}

/// Asserts that the specified type is a valid *closed* session type (that is, it does not
/// `Continue` outside itself).
#[cfg(test)]
#[macro_export]
macro_rules! assert_all_closed_sessions {
    () => {};
    ($session:ty) => { assert_all_closed_sessions!($session,) };
    ($session:ty, $($rest:tt)*) => {
        const _: fn() = || {
            fn assert_impl_all<T>()
            where
                T: $crate::Session,
                <T as Session>::Dual: $crate::Session,
            {
            }
            assert_impl_all::<$session>();
        };
        assert_all_closed_sessions!($($rest)*);
    };
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    // The session type `P` incorporates every construct in the session type language. This unit
    // test assures that even a complex nested session type is still a ZST.
    #[test]
    fn complex_session_zero_size() {
        type P = Loop<
            Loop<
                Choose<(
                    Send<usize, Continue>,
                    Recv<String, Continue>,
                    Offer<(
                        Send<bool, Continue>,
                        Continue<S<Z>>,
                        Split<Send<isize, Continue>, Recv<isize, Continue>>,
                    )>,
                )>,
            >,
        >;
        assert_eq!(std::mem::size_of::<P>(), 0);
    }
}
