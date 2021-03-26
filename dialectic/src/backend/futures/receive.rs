use crate::backend;
use std::{
    fmt::Debug,
    future::Future,
    marker::PhantomData,
    pin::Pin,
    task::{Context, Poll},
};

/// The future returned from [`ReceiveExt::recv`](crate::backend::ReceiveExt::recv).
#[derive(Debug)]
#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct Receive<'a, Rx: ?Sized, T> {
    rx: &'a mut Rx,
    ty: PhantomData<fn() -> T>,
}

impl<'a, Rx: ?Sized, T> Receive<'a, Rx, T> {
    pub(crate) fn new(rx: &'a mut Rx) -> Receive<'a, Rx, T> {
        Self {
            rx,
            ty: PhantomData,
        }
    }
}

impl<Rx: Unpin + ?Sized, T> Unpin for Receive<'_, Rx, T> {}

impl<T, Rx: backend::Receive<T> + Unpin + ?Sized> Future for Receive<'_, Rx, T> {
    type Output = Result<T, Rx::Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        backend::Receive::<T>::poll_recv(Pin::new(self.rx), cx)
    }
}
