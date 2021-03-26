use crate::backend::{self, By};
use futures::ready;
use std::{
    fmt::Debug,
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

/// The future returned from [`TransmitExt::send`](crate::backend::TransmitExt::send).
#[derive(derivative::Derivative)]
#[derivative(Debug(bound = "Tx: Debug, <T as By<'b, Tx::Convention>>::Type: Debug"))]
#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct Send<'a, 'b: 'a, Tx: ?Sized, T>
where
    Tx: backend::Transmit<T>,
    T: By<'b, Tx::Convention>,
{
    tx: &'a mut Tx,
    message: Option<<T as By<'b, Tx::Convention>>::Type>,
}

impl<'a, 'b: 'a, Tx, T> Unpin for Send<'a, 'b, Tx, T>
where
    Tx: backend::Transmit<T> + Unpin + ?Sized,
    T: By<'b, Tx::Convention>,
{
}

impl<'a, 'b, Tx, T> Send<'a, 'b, Tx, T>
where
    Tx: backend::Transmit<T> + ?Sized,
    T: By<'b, Tx::Convention>,
{
    pub(crate) fn new(
        tx: &'a mut Tx,
        message: <T as By<'b, Tx::Convention>>::Type,
    ) -> Send<'a, 'b, Tx, T> {
        Self {
            tx,
            message: Some(message),
        }
    }
}

impl<'a, 'b, Tx, T> Send<'a, 'b, Tx, T>
where
    Tx: backend::Transmit<T> + Unpin + ?Sized,
    T: By<'b, Tx::Convention>,
{
    pub(crate) fn tx_pin_mut(&mut self) -> Pin<&mut Tx> {
        Pin::new(self.tx)
    }
}

impl<'a, 'b: 'a, T, Tx> Future for Send<'a, 'b, Tx, T>
where
    Tx: backend::Transmit<T> + Unpin + ?Sized,
    T: By<'b, Tx::Convention>,
{
    type Output = Result<(), Tx::Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        ready!(this.tx_pin_mut().as_mut().poll_ready(cx))?;
        let message = this.message.take().expect("polled `Send` after completion");
        this.tx_pin_mut().start_send(message)?;
        Poll::Ready(Ok(()))
    }
}
