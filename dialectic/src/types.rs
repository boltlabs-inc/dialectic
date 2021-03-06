//! The structs in this module enumerate the shapes of all expressible sessions.
//!
//! 💡 Generally speaking, users of Dialectic (that's you!) don't need to write these types
//! directly; it's more readable to instead use the [`Session!`](macro@crate::Session) macro to
//! specify a session type.
//!
//! The traits in this module are used to implement the session type system rules, and generally do
//! not need to be referred to directly from code that uses this library. All of them are sealed, so
//! that they can only ever apply to the session types defined in this crate. If you want to state
//! the bound that a type is a session type, use the [`Session`](trait@crate::Session) trait rather
//! than any of these bounds individually.

use crate::unary::{LessThan, Unary, S, Z};

// Each construct in the session types language lives in its own module, along with the
// implementation of its various typing rules.
mod call;
mod choose;
mod r#continue;
mod done;
mod r#loop;
mod offer;
mod recv;
mod send;
mod split;

pub use call::*;
pub use choose::*;
pub use done::*;
pub use offer::*;
pub use r#continue::*;
pub use r#loop::*;
pub use recv::*;
pub use send::*;
pub use split::*;

/// Each session type has a [`HasDual::DualSession`], the type of the corresponding client on the
/// other side of the channel. The sealed trait `HasDual` enumerates these types, and provides the
/// dual of each.
///
/// 💡 In general, you should prefer the [`Session`]() trait to the [`HasDual`] trait, since
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
/// type Client = Loop<Offer<(Split<Call<Send<String, Done>, Done>, Recv<usize, Done>, Done>, Recv<bool, Continue<0>>)>>;
/// type Server = Loop<Choose<(Split<Send<usize, Done>, Call<Recv<String, Done>, Done>, Done>, Send<bool, Continue<0>>)>>;
///
/// assert_type_eq_all!(Client, <Server as HasDual>::DualSession);
/// ```
///
/// [`Session`]: trait@crate::Session
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
///
/// [`Session`]: trait@crate::Session
pub trait Actionable: sealed::IsSession {
    /// The next actual channel action, which must be one of [`Send`], [`Recv`], [`Offer`],
    /// [`Choose`], [`Split`], [`Call`], or [`Done`].
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
///
/// [`Session`]: trait@crate::Session
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
///     <Send<i64, Continue<0>> as Subst<Recv<(), Done>>>::Substituted,
///     Send<i64, Recv<(), Done>>,
/// );
/// ```
pub trait Subst<P, N: Unary = Z>: sealed::IsSession {
    /// The result of the substitution.
    type Substituted: 'static;
}

/// Substitute `P` for every [`Done`] in `Self`, thus concatenating the session `P` to `Self`.
///
/// This does not require `P` to be a closed session type; it is reasonable to use open session
/// types (those with [`Continue`]s that refer outside of themselves) as `P`. [`Then`] is careful to
/// adjust the indices of such open types so that they refer correctly outside of `Self` even when
/// `Self` contains [`Loop`]s.
pub trait Then<P, N: Unary = Z>: sealed::IsSession {
    /// The combined type resulting from substituting `P` for [`Done`] in `Self`.
    type Combined: 'static;
}

/// For every "open" [`Continue`] (i.e. with an index larger than the number of [`Loop`]s that
/// contain it), increment its index by `N`.
///
/// This is used internally in the [`Then`] implementation for [`Done`].
pub trait Lift<N: Unary, Level: Unary = Z>: sealed::IsSession {
    /// The result of the lifting operation.
    type Lifted: 'static;
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

/// Analogously to [`EachSubst`], this trait allows iteration/mapping of the [`Then`] transform
/// over a type level list.
pub trait EachThen<P, N: Unary = Z>: sealed::EachSession {
    /// The result of the map.
    type Combined: 'static;
}

impl<N: Unary, Q> EachThen<Q, N> for () {
    type Combined = ();
}

impl<N: Unary, Q, P, Ps> EachThen<Q, N> for (P, Ps)
where
    P: Then<Q, N>,
    Ps: EachThen<Q, N>,
{
    type Combined = (P::Combined, Ps::Combined);
}

/// Analogously to [`EachSubst`], this trait allows iteration/mapping of the [`Then`] transform
/// over a type level list.
pub trait EachLift<N: Unary, Level: Unary = Z>: sealed::EachSession {
    /// The result of the map.
    type Lifted: 'static;
}

impl<N: Unary, Level: Unary> EachLift<N, Level> for () {
    type Lifted = ();
}

impl<N: Unary, Level: Unary, P, Ps> EachLift<N, Level> for (P, Ps)
where
    P: Lift<N, Level>,
    Ps: EachLift<N, Level>,
{
    type Lifted = (P::Lifted, Ps::Lifted);
}

/// Select by index from a type level list.
///
/// This is used internally by the [`Choose`], [`Offer`], and [`Continue`] session types.
///
/// # Examples
///
/// ```
/// use static_assertions::{assert_impl_all, assert_not_impl_any};
/// use dialectic::types::*;
/// use dialectic::unary::UnaryOf;
///
/// type L = (UnaryOf<0>, (UnaryOf<1>, (UnaryOf<2>, ())));
///
/// assert_impl_all!(L: Select<UnaryOf<0>, Selected = UnaryOf<0>, Remainder = (UnaryOf<1>, (UnaryOf<2>, ()))>);
/// assert_impl_all!(L: Select<UnaryOf<1>, Selected = UnaryOf<1>, Remainder = (UnaryOf<0>, (UnaryOf<2>, ()))>);
/// assert_impl_all!(L: Select<UnaryOf<2>, Selected = UnaryOf<2>, Remainder = (UnaryOf<0>, (UnaryOf<1>, ()))>);
///
/// assert_not_impl_any!(L: Select<UnaryOf<3>>);
/// ```
pub trait Select<N: Unary>: sealed::Select<N> {
    /// The thing which is selected from this list by the index `N`.
    type Selected;

    /// The list with the selected thing removed.
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
    type Remainder = (T, <(P, Rest) as Select<N>>::Remainder);
}

mod sealed {
    use std::any::Any;

    use super::*;

    /// Seal the [`Session`] trait so only types defined in this crate can be session types.
    pub trait IsSession: Any + Default {}

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
                    Send<usize, Continue<0>>,
                    Recv<String, Continue<0>>,
                    Offer<(
                        Send<bool, Continue<0>>,
                        Continue<1>,
                        Split<Send<isize, Continue<0>>, Recv<isize, Continue<0>>, Done>,
                    )>,
                )>,
            >,
        >;
        assert_eq!(std::mem::size_of::<P>(), 0);
    }

    include!(concat!(env!("OUT_DIR"), "/valid_sessions.rs"));
}
