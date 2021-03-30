use crate::backend;
use std::{
    fmt::Debug,
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

/// The future returned from [`ReceiveExt::recv`](crate::backend::ReceiveExt::recv).
#[derive(Debug)]
#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct ReceiveChoice<'a, Rx: ?Sized, const LENGTH: usize> {
    rx: &'a mut Rx,
}

impl<'a, Rx: ?Sized, const LENGTH: usize> ReceiveChoice<'a, Rx, LENGTH> {
    pub(crate) fn new(rx: &'a mut Rx) -> ReceiveChoice<'a, Rx, LENGTH> {
        Self { rx }
    }
}

impl<Rx: Unpin + ?Sized, const LENGTH: usize> Unpin for ReceiveChoice<'_, Rx, LENGTH> {}

impl<Rx: backend::ReceiveChoice + Unpin + ?Sized, const LENGTH: usize> Future
    for ReceiveChoice<'_, Rx, LENGTH>
{
    type Output = Result<backend::Choice<LENGTH>, Rx::Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        backend::ReceiveChoice::poll_recv_choice(Pin::new(self.rx), cx)
    }
}
