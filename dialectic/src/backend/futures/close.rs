use crate::backend;
use std::{
    fmt::Debug,
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

/// The future returned from [`TransmitterExt::close`](crate::backend::TransmitterExt::close).
#[derive(Debug)]
#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct Close<'a, Tx: ?Sized> {
    tx: &'a mut Tx,
}

impl<'a, Tx: ?Sized> Close<'a, Tx> {
    pub(crate) fn new(tx: &'a mut Tx) -> Close<'a, Tx> {
        Self { tx }
    }
}

impl<Tx: Unpin + ?Sized> Unpin for Close<'_, Tx> {}

impl<Tx: backend::Transmitter + Unpin + ?Sized> Future for Close<'_, Tx> {
    type Output = Result<(), Tx::Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        backend::Transmitter::poll_close(Pin::new(self.tx), cx)
    }
}
