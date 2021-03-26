use crate::backend::{self, Choice};
use futures::ready;
use std::{
    fmt::Debug,
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

/// A future that sends a single [`Choice`] over `Tx` (internal implementation detail).
#[derive(Debug)]
#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct SendChoice<'a, Tx: ?Sized, const LENGTH: usize> {
    tx: &'a mut Tx,
    choice: Option<Choice<LENGTH>>,
}

impl<'a, Tx: ?Sized, const LENGTH: usize> Unpin for SendChoice<'a, Tx, LENGTH> {}

impl<'a, Tx: ?Sized, const LENGTH: usize> SendChoice<'a, Tx, LENGTH> {
    pub(crate) fn new(tx: &'a mut Tx, choice: Choice<LENGTH>) -> SendChoice<'a, Tx, LENGTH> {
        Self {
            tx,
            choice: Some(choice),
        }
    }
}

impl<'a, Tx: Unpin + ?Sized, const LENGTH: usize> SendChoice<'a, Tx, LENGTH> {
    pub(crate) fn tx_pin_mut(&mut self) -> Pin<&mut Tx> {
        Pin::new(self.tx)
    }
}

impl<'a, Tx, const LENGTH: usize> Future for SendChoice<'a, Tx, LENGTH>
where
    Tx: backend::TransmitChoice + Unpin + ?Sized,
{
    type Output = Result<(), Tx::Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        ready!(this.tx_pin_mut().as_mut().poll_ready(cx))?;
        let choice = this
            .choice
            .take()
            .expect("polled `SendChoice` after completion");
        this.tx_pin_mut().start_send_choice(choice)?;
        Poll::Ready(Ok(()))
    }
}
