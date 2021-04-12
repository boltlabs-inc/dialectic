//! The interface implemented by all transport backends for a [`Chan`](crate::Chan).
//!
//! A [`Chan<S, Tx, Rx>`](crate::Chan) is parameterized by its transmitting channel `Tx` and its
//! receiving channel `Rx`. In order to use a `Chan` to run a session, these underlying channels
//! must implement the traits [`Transmitter`] and [`Receiver`], as well as [`Transmit<T>`](Transmit)
//! and [`Receive<T>`](Receive) for at least the types `T` used in those capacities in any given
//! session.
//!
//! Functions which are generic over their backend will in turn need to specify the bounds
//! [`Transmit<T>`](Transmit) and [`Receive<T>`](Receive) for all `T`s they send and receive,
//! respectively. The [`Transmitter`](macro@crate::Transmitter) and
//! [`Receiver`](macro@crate::Receiver) attribute macros make this bound specification succinct; see
//! their documentation for more details.

#[doc(no_inline)]
pub use call_by::{By, Convention, Mut, Ref, Val};
use std::{
    convert::{TryFrom, TryInto},
    future::Future,
    pin::Pin,
};
pub use vesta::{Case, Match};

mod choice;
pub use choice::*;

/// A backend transport used for transmitting (i.e. the `Tx` parameter of [`Chan`](crate::Chan))
/// must implement [`Transmitter`], which specifies what type of errors it might return, as well as
/// giving a method to send [`Choice`]s across the channel. This is a super-trait of [`Transmit`],
/// which is what's actually needed to receive particular values over a [`Chan`](crate::Chan).
///
/// If you're writing a function and need a lot of different [`Transmit<T>`](Transmit) bounds, the
/// [`Transmitter`](macro@crate::Transmitter) attribute macro can help you specify them more
/// succinctly.
pub trait Transmitter {
    /// The type of possible errors when sending.
    type Error;
}

/// A marker trait indicating that some type `T` is transmittable as the associated type
/// `ReceivedAs` in a session type specification.
///
/// This type is blanket-implemented for *all* sized types such that they are transmitted as
/// themselves; for unsized types such as `str` and `[T]`, there are implementations such that `str`
/// is transmittable and `ReceivedAs = String` and `[T]` is transmittable and `ReceivedAs = Vec<T>`.
/// Backends must choose whether or not to implement `Transmit` for these unsized types, but *must*
/// guarantee that they can be received as their associated `ReceivedAs` type if they are
/// transmittable.
pub trait Transmittable {
    /// The equivalent type that receiving this transmitted type will give on the other end of the
    /// connection.
    type ReceivedAs: Sized;
}

impl<T: Sized> Transmittable for T {
    type ReceivedAs = T;
}

impl Transmittable for str {
    type ReceivedAs = String;
}

impl Transmittable for std::ffi::CStr {
    type ReceivedAs = std::ffi::CString;
}

impl Transmittable for std::ffi::OsStr {
    type ReceivedAs = std::ffi::OsString;
}

impl Transmittable for std::path::Path {
    type ReceivedAs = std::path::PathBuf;
}

impl<T> Transmittable for [T] {
    type ReceivedAs = Vec<T>;
}

/// If a transport is [`Transmit<T, C>`](Transmit), we can use it to [`send`](Transmit::send) a
/// message of type `T` by [`Val`], [`Ref`], or [`Mut`], depending on the calling convention
/// specified by `C`. Any transmitted type `T` may be received as its associated [`<T as
/// Transmittable>::ReceivedAs`](Transmittable) type; if a backend has `Transmit<T, C>` implemented
/// for some `T`, it guarantees that `T` will be received on the other side as an equivalent,
/// well-formed `T::ReceivedAs`.
///
/// If you're writing a function and need a lot of different `Transmit<T, C>` bounds, the
/// [`Transmitter`](macro@crate::Transmitter) attribute macro can help you specify them more
/// succinctly.
///
/// # Examples
///
/// For an example of implementing [`Transmit`], check out the source for the implementation of
/// [`Transmit`] for the [`dialectic_tokio_mpsc::Sender`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
///
/// [`dialectic_tokio_mpsc::Sender`]:
/// https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Sender.html
pub trait Transmit<T: ?Sized, C: Convention = Val>: Transmitter
where
    T: Transmittable,
{
    /// Send a message using the [`Convention`] specified by the trait implementation.
    fn send<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <T as By<'a, C>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: By<'a, C>,
        'a: 'async_lifetime;
}

/// A trait describing how we can transmit a [`Choice<N>`](Choice) for any `N: usize`. If your
/// backend should be able to transmit any [`Choice<N>`](Choice), this is the trait to implement;
/// you'll get a blanket impl from it for `TransmitCase<Choice<N>>`.
pub trait TransmitChoice: Transmitter {
    /// Send a [`Choice<N>`](Choice) over the backend, for some `N`.
    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>;
}

/// If a transport is [`TransmitCase<T, C>`](TransmitCase), we can use it to
/// [`send_case`](TransmitCase::send_case) a message of type `T` by [`Val`], [`Ref`], or [`Mut`],
/// depending on the calling convention specified by `C`. [`TransmitCase`] is highly similar to
/// [`Transmit`], with a major difference: [`TransmitCase`] may be used to send only *part* of an
/// `enum` datatype, as part of a [`Chan::choose`](crate::Chan::choose) call. This is because the
/// discriminant is actually the constant `N: usize` parameter to the [`TransmitCase::send_case`]
/// method. This matching/construction/deconstruction is done through the
/// [`vesta`](https://docs.rs/vesta) crate and its `Match` and `Case` traits; implementation of
/// these traits does not need to be done by hand as Vesta provides a derive macro for them.
///
/// You do not need to ever implement `TransmitCase`. It has blanket implementations for all types
/// that matter.
pub trait TransmitCase<T: ?Sized, C: Convention, const N: usize>:
    Transmitter + sealed::TransmitCase<T, C>
where
    T: Transmittable + Match,
{
    /// Send a "case" of a [`Match`]-able type.
    fn send_case<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <<T as Case<N>>::Case as By<'a, C>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: Case<N>,
        <T as Case<N>>::Case: By<'a, C>,
        'a: 'async_lifetime;
}

/// This is a wrapper type which disambiguates, at the type level, "custom" choice types, and makes
/// sure Rust sees them as a different type from `Choice<N>`.
#[derive(Debug)]
pub struct CustomChoice<T>(pub T);

unsafe impl<T: Match> Match for CustomChoice<T> {
    type Range = T::Range;

    fn tag(&self) -> Option<usize> {
        self.0.tag()
    }
}

impl<T: Match + Case<N>, const N: usize> Case<N> for CustomChoice<T> {
    type Case = T::Case;

    unsafe fn case(this: Self) -> Self::Case {
        T::case(this.0)
    }

    fn uncase(case: Self::Case) -> Self {
        CustomChoice(T::uncase(case))
    }
}

impl<Tx: Transmitter + TransmitChoice, const LENGTH: usize, const N: usize>
    TransmitCase<Choice<LENGTH>, Val, N> for Tx
{
    fn send_case<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        _message: <<Choice<LENGTH> as Case<N>>::Case as By<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        Choice<LENGTH>: Case<N>,
        <Choice<LENGTH> as Case<N>>::Case: By<'a, Val>,
        'a: 'async_lifetime,
    {
        // FIXME(sleffy): no unwrap
        self.send_choice::<LENGTH>(
            TryFrom::<u8>::try_from(N.try_into().unwrap()).expect("N < LENGTH by trait properties"),
        )
    }
}

impl<Tx: Transmitter + Transmit<T>, T: Match + Transmittable, const N: usize>
    TransmitCase<CustomChoice<T>, Val, N> for Tx
{
    fn send_case<'a, 'async_lifetime>(
        &'async_lifetime mut self,
        message: <<CustomChoice<T> as Case<N>>::Case as By<'a, Val>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        CustomChoice<T>: Case<N>,
        <CustomChoice<T> as Case<N>>::Case: By<'a, Val>,
        'a: 'async_lifetime,
    {
        self.send(<CustomChoice<T> as Case<N>>::uncase(call_by::coerce_move(message)).0)
    }
}

/// A backend transport used for receiving (i.e. the `Rx` parameter of [`Chan`](crate::Chan)) must
/// implement [`Receiver`], which specifies what type of errors it might return. This is a
/// super-trait of [`Receive`] and [`ReceiveCase`], which are the traits that are actually needed to
/// receive particular values over a [`Chan`](crate::Chan).
///
/// If you're writing a function and need a lot of different [`Receive<T>`](Receive) bounds, the
/// [`Receiver`](macro@crate::Receiver) attribute macro can help you specify them more succinctly.
pub trait Receiver {
    /// The type of possible errors when receiving.
    type Error;
}

/// If a transport is [`Receive<T>`](Receive), we can use it to [`recv`](Receive::recv) a message of
/// type `T`.
///
/// If you're writing a function and need a lot of different [`Receive<T>`](Receive) bounds, the
/// [`Receiver`](macro@crate::Receiver) attribute macro can help you specify them more succinctly.
///
/// # Examples
///
/// For an example of implementing [`Receive`], check out the source for the implementation of
/// [`Receive`] for the [`dialectic_tokio_mpsc::Receiver`] type in the [`dialectic_tokio_mpsc`]
/// crate.
///
/// [`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
///
/// [`dialectic_tokio_mpsc::Receiver`]:
/// https://docs.rs/dialectic-tokio-mpsc/latest/dialectic_tokio_mpsc/struct.Receiver.html
pub trait Receive<T>: Receiver {
    /// Receive a message. This may require type annotations for disambiguation.
    fn recv<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>>;
}

/// A trait describing how a receiver can receive a `Choice<N>` for any `N`. Implementing this trait
/// is sufficient to get a blanket impl of `ReceiveCase<Choice<N>>` for all `N` across your receiver
/// type.
///
/// [`ReceiveCase`] and [`ReceiveChoice`] exist due to current Rust limitations on quantifying over
/// const generics and type parameters; if we had rank-2 types, they would not be necessary.
pub trait ReceiveChoice: Receiver {
    /// Receive any `Choice<N>`. It is impossible to construct a `Choice<0>`, so if `N = 0`, a
    /// [`Receiver::Error`] must be returned.
    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>;
}

/// A trait describing how a receiver can receive some `T` which can be matched on and has one or
/// more associated "cases". This casing/matching is done through the
/// [`vesta`](https://docs.rs/vesta) crate's [`Match`] and [`Case`] traits, which can be derived for
/// some types through Vesta's `Match` derive macro.
///
/// If you are writing a backend for a new protocol, you almost certainly do not need to implement
/// [`ReceiveCase`] for your backend, and you can rely on an implementation of [`ReceiveChoice`]. If
/// you are writing a backend for an existing protocol which you are reimplementing using Dialectic,
/// however, then [`ReceiveCase`] is necessary to allow you to branch on a custom type.
///
/// If you're writing a function and need a lot of different `ReceiveCase<T, C>` bounds, the
/// [`Receiver`](macro@crate::Receiver) attribute macro can help you specify them more succinctly.
pub trait ReceiveCase<T>: Receiver
where
    T: Match,
{
    /// Receive a case of some type which implements [`Match`]. This may require type annotations
    /// for disambiguation.
    fn recv_case<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<T, Self::Error>> + Send + 'async_lifetime>>;
}

impl<Rx: Receiver, const LENGTH: usize> ReceiveCase<Choice<LENGTH>> for Rx
where
    Rx: ReceiveChoice,
{
    fn recv_case<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>
    {
        self.recv_choice::<LENGTH>()
    }
}

impl<Rx: Receiver, T: Match> ReceiveCase<CustomChoice<T>> for Rx
where
    Rx: Receive<T> + Send,
{
    fn recv_case<'async_lifetime>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<CustomChoice<T>, Self::Error>> + Send + 'async_lifetime>>
    {
        Box::pin(async move {
            let t = self.recv().await?;
            Ok(CustomChoice(t))
        })
    }
}

mod sealed {
    use super::*;

    pub trait TransmitCase<T: ?Sized, C> {}

    impl<Tx: Transmitter + Transmit<T>, T: Transmittable> TransmitCase<CustomChoice<T>, Val> for Tx {}
    impl<Tx: Transmitter + TransmitChoice, const N: usize> TransmitCase<Choice<N>, Val> for Tx {}

    pub trait ReceiveCase<T: ?Sized> {}

    impl<Rx: Receiver + Receive<T>, T> ReceiveCase<CustomChoice<T>> for Rx {}
    impl<Rx: Receiver + ReceiveChoice, const N: usize> ReceiveCase<Choice<N>> for Rx {}
}
