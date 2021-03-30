use crate::backend;
use std::{
    fmt::Debug,
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

/// The future returned from [`TransmitterExt::flush`](crate::backend::TransmitterExt::flush).
#[derive(Debug)]
#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct Flush<'a, Tx: ?Sized> {
    tx: &'a mut Tx,
}

impl<'a, Tx: ?Sized> Flush<'a, Tx> {
    pub(crate) fn new(tx: &'a mut Tx) -> Flush<'a, Tx> {
        Self { tx }
    }
}

impl<Tx: Unpin + ?Sized> Unpin for Flush<'_, Tx> {}

impl<Tx: backend::Transmitter + Unpin + ?Sized> Future for Flush<'_, Tx> {
    type Output = Result<(), Tx::Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        backend::Transmitter::poll_flush(Pin::new(self.tx), cx)
    }
}
