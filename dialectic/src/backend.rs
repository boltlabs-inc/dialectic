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

pub trait TransmitChoice: Transmitter {
    fn send_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
        choice: Choice<LENGTH>,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>;
}

pub trait TransmitCase<T: ?Sized, C: Convention = Val>: Transmitter
where
    T: Transmittable + Match,
{
    fn send_case<'a, 'async_lifetime, const N: usize>(
        &'async_lifetime mut self,
        message: <<T as Case<N>>::Case as By<'a, C>>::Type,
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send + 'async_lifetime>>
    where
        T: Case<N>,
        <T as Case<N>>::Case: By<'a, C>,
        'a: 'async_lifetime;
}

impl<Tx: Transmitter + TransmitChoice, const LENGTH: usize> TransmitCase<Choice<LENGTH>, Val>
    for Tx
{
    fn send_case<'a, 'async_lifetime, const N: usize>(
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

/// A backend transport used for receiving (i.e. the `Rx` parameter of [`Chan`](crate::Chan)) must
/// implement [`Receiver`], which specifies what type of errors it might return, as well as giving a
/// method to send [`Choice`]s across the channel. This is a super-trait of [`Receive`], which is
/// what's actually needed to receive particular values over a [`Chan`](crate::Chan).
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

pub trait ReceiveChoice: Receiver {
    /// Receive any `Choice<N>`. It is impossible to construct a `Choice<0>`, so if `N = 0`, a
    /// [`Receiver::Error`] must be returned.
    fn recv_choice<'async_lifetime, const LENGTH: usize>(
        &'async_lifetime mut self,
    ) -> Pin<Box<dyn Future<Output = Result<Choice<LENGTH>, Self::Error>> + Send + 'async_lifetime>>;
}

pub trait ReceiveCase<T>: Receiver {
    /// Receive a choice. This may require type annotations for disambiguation.
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
