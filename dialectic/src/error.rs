use std::{
    convert::Infallible,
    pin::Pin,
    task::{Context, Poll},
};

use crate::backend::Transmitter;
#[allow(unused_imports)] // To link with documentation
use crate::prelude::*;

/// A placeholder for a missing [`Transmit`] or [`Receive`] end of a connection.
///
/// When using [`split`](Chan::split), the resultant two channels can only send or only receive,
/// respectively. This is reflected at the type level by the presence of [`Unavailable`] on the type
/// of the connection which *is not* present for each part of the split.
///
/// [`Unavailable`] *does* implement [`Transmitter`] and [`Receiver`] (which is necessary so that it
/// can be a backend type in a [`Chan`]), but it *does not* implement [`ReceiveChoice`],
/// [`TransmitChoice`], [`Receive`], or [`Transmit`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Unavailable {
    _priv: (),
}

impl Transmitter for Unavailable {
    type Error = Infallible;
    type Convention = Val;

    fn poll_ready(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_close(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }
}

impl Receiver for Unavailable {
    type Error = Infallible;
}

/// The error returned when a closure which is expected to complete a channel's session fails to
/// finish the session of the channel it is given.
///
/// This error can arise either if the channel is dropped *before* its session is completed, or if
/// it is stored somewhere and is dropped *after* the closure's future is finished. The best way to
/// ensure this error does not occur is to call [`close`](Chan::close) on the channel,
/// which statically ensures it is dropped exactly when the session is complete.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum SessionIncomplete<Tx, Rx> {
    /// Both the sending half `Tx` and the receiving half `Rx` did not complete the session
    /// correctly.
    BothHalves {
        /// The incomplete sending half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        tx: IncompleteHalf<Tx>,
        /// The incomplete receiving half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        rx: IncompleteHalf<Rx>,
    },
    /// Only the sending half `Tx` did not complete the session correctly, but the receiving half
    /// `Rx` did complete it correctly.
    TxHalf {
        /// The incomplete sending half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        tx: IncompleteHalf<Tx>,
        /// The receiving half, whose session was completed.
        #[derivative(Debug = "ignore")]
        rx: Rx,
    },
    /// Only the receiving half `Rx` did not complete the session correctly, but the sending half
    /// `Tx` did complete it correctly.
    RxHalf {
        /// The sending half, whose session was completed.
        #[derivative(Debug = "ignore")]
        tx: Tx,
        /// The incomplete receiving half: [`Unfinished`](IncompleteHalf::Unfinished) if dropped
        /// before the end of the session, [`Unclosed`](IncompleteHalf::Unclosed) if not dropped
        /// after the end of the session.
        rx: IncompleteHalf<Rx>,
    },
}

/// A representation of what has gone wrong when a connection half `Tx` or `Rx` is incomplete.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum IncompleteHalf<T> {
    /// The underlying channel was dropped before the session was `Done`.
    Unfinished(#[derivative(Debug = "ignore")] T),
    /// The underlying channel was not dropped or [`close`](Chan::close)d after the session
    /// was `Done`.
    Unclosed,
}

impl<T> std::error::Error for IncompleteHalf<T> {}

impl<T> std::fmt::Display for IncompleteHalf<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "incomplete session or sub-session: channel half ")?;
        write!(
            f,
            "{}",
            match self {
                IncompleteHalf::Unfinished(_) => "was dropped before the session was `Done`",
                IncompleteHalf::Unclosed => "was not closed after the session was `Done`",
            }
        )
    }
}

impl<Tx, Rx> SessionIncomplete<Tx, Rx> {
    /// Extract the send and receive halves `Tx` and `Rx`, if they are present, from this
    /// `SessionIncomplete` error.
    pub fn into_halves(
        self,
    ) -> (
        Result<Tx, IncompleteHalf<Tx>>,
        Result<Rx, IncompleteHalf<Rx>>,
    ) {
        match self {
            SessionIncomplete::BothHalves { tx, rx } => (Err(tx), Err(rx)),
            SessionIncomplete::TxHalf { tx, rx } => (Err(tx), Ok(rx)),
            SessionIncomplete::RxHalf { tx, rx } => (Ok(tx), Err(rx)),
        }
    }
}

impl<Tx, Rx> std::error::Error for SessionIncomplete<Tx, Rx> {}

impl<Tx, Rx> std::fmt::Display for SessionIncomplete<Tx, Rx> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use IncompleteHalf::*;
        write!(f, "incomplete session or sub-session: channel")?;
        let reason = match self {
            SessionIncomplete::BothHalves { tx, rx } => match (tx, rx) {
                (Unclosed, Unclosed) => " was not closed after the session was `Done`",
                (Unclosed, Unfinished(_)) => {
                    "'s sending half was not closed after the session was `Done` \
                    and its receiving half was dropped before the session was `Done`"
                }
                (Unfinished(_), Unclosed) => {
                    "'s sending half was dropped before the session was `Done` \
                    and its receiving half was not closed after the session was `Done`"
                }
                (Unfinished(_), Unfinished(_)) => " was dropped before the session was `Done`",
            },
            SessionIncomplete::TxHalf { tx, .. } => match tx {
                Unfinished(_) => "'s sending half was dropped before the session was `Done`",
                Unclosed => "'s sending half was not closed after the session was `Done`",
            },
            SessionIncomplete::RxHalf { rx, .. } => match rx {
                Unfinished(_) => "'s receiving half was dropped before the session was `Done`",
                Unclosed => "'s receiving half was not closed after the session was `Done`",
            },
        };
        write!(f, "{}", reason)
    }
}
