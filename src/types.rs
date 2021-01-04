//! The types in this module enumerate the shapes of all expressible sessions.

use super::*;
pub use unary::types::*;
pub use unary::{constants, LessThan, Unary, S, Z};

pub mod tuple;
pub mod unary;

// Each construct in the session types language lives in its own module, along with the
// implementation of its various typing rules.
mod r#break;
mod choose;
mod r#continue;
mod done;
mod r#loop;
mod offer;
mod recv;
mod send;
mod split;

pub use choose::*;
pub use done::*;
pub use offer::*;
pub use r#break::*;
pub use r#continue::*;
pub use r#loop::*;
pub use recv::*;
pub use send::*;
pub use split::*;

/// A session type describes the sequence of operations performed by one end of a bidirectional
/// [`Chan`].
///
/// Each session type has a [`Session::Dual`], the type of the corresponding client on the other
/// side of the channel. The sealed trait `Session` enumerates these types, and provides the dual of
/// each.
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
/// assert_type_eq_all!(Client, <Server as Session>::Dual);
/// ```
pub trait Session: Sized + sealed::IsSession {
    /// The dual to this session type, i.e. the session type required of the other end of the
    /// channel.
    type Dual: Session<Dual = Self>;
}

/// The [`Actionable`] trait infers the next action necessary on a channel, automatically stepping
/// through [`Loop`]s and [`Continue`]sion points.
pub trait Actionable<E = ()>: Session
where
    E: Environment,
    Self: Scoped<E::Depth>,
{
    /// The next actual channel action: [`Send`], [`Recv`], [`Offer`], [`Choose`], or [`Split`].
    /// This steps through [`Loop`] and [`Continue`] transparently.
    ///
    /// The constraints on this associated type ensure that it is idemopotent: the `Action` and
    /// `Env` of an `Action` are the same as those of that `Action`.
    type Action: Actionable<Self::Env, Action = Self::Action, Env = Self::Env>;

    /// The environment resulting from stepping through one or many [`Loop`] or [`Continue`] points
    /// to the next real channel action.
    type Env: Environment;
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols [`Session`].
pub trait EachSession: Sized + sealed::EachSession
where
    Self::Dual: EachSession<Dual = Self>,
{
    /// The point-wise [`Session::Dual`] of a type-level list of session types.
    type Dual;
}

impl EachSession for () {
    type Dual = ();
}

impl<P, Ps> EachSession for (P, Ps)
where
    P: Session,
    Ps: EachSession,
{
    type Dual = (P::Dual, Ps::Dual);
}

/// In the [`Choose`] and [`Offer`] session types, we provide the ability to choose/offer a list of
/// protocols. The sealed [`EachSession`] trait ensures that every protocol in a type level list of
/// protocols is [`Actionable`].
pub trait EachActionable<E = ()>: EachSession
where
    E: Environment,
{
}

impl<E> EachActionable<E> for () where E: Environment {}

impl<E, P, Ps> EachActionable<E> for (P, Ps)
where
    P: Actionable<E>,
    Ps: EachActionable<E>,
    E: Environment,
{
}

/// A valid session environment is a type-level list of session types, each of which may refer by
/// [`Continue`] index to any other session in the list which is *below or including* itself.
pub trait Environment {
    /// The depth of a session environment is the number of loops to which a [`Continue`] could
    /// jump, i.e. the number of session types in the session environment.
    type Depth: Unary;
}

impl Environment for () {
    type Depth = Z;
}

impl<P, Ps> Environment for (P, Ps)
where
    P: Scoped<S<Ps::Depth>>,
    Ps: Environment,
{
    type Depth = S<Ps::Depth>;
}

/// A session type is *scoped* for a given environment depth `N` if it [`Continue`]s no more than
/// `N` [`Loop`] levels above itself.
///
/// A session type is `Scoped<Z>` (which can be abbreviated `Scoped`) if it does not [`Continue`] to
/// any loop above itself, i.e. all `Continue<N>` refer to a loop which they themselves are within.
pub trait Scoped<N: Unary = Z>: Session {}

/// In the [`Choose`] and [`Offer`] session types, `EachScoped<N>` is used to assert that every
/// choice or offering is [`Scoped`].
pub trait EachScoped<N: Unary = Z>: EachSession {}
impl<N: Unary> EachScoped<N> for () {}
impl<N: Unary, P: Scoped<N>, Ps: EachScoped<N>> EachScoped<N> for (P, Ps) {}

/// Select by index from a type level list.
///
/// This is used internally by the [`Choose`], [`Offer`], and [`Continue`] session types.
///
/// # Examples
///
/// ```
/// # use static_assertions::assert_type_eq_all;
/// use dialectic::Select;
/// use dialectic::unary::types::*;
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
    use super::*;

    /// Seal the [`Session`] trait so only types defined in this crate can be session types.
    pub trait IsSession {}

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
                T: $crate::NewSession,
                T::Dual: $crate::Actionable,
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
                    Send<usize>,
                    Recv<String>,
                    Offer<(Send<bool>, Continue<S<Z>>, Split<Send<isize>, Recv<isize>>)>,
                )>,
            >,
        >;
        assert_eq!(std::mem::size_of::<P>(), 0);
    }
}
