use dialectic::{prelude::*, IncompleteHalf, SessionIncomplete};
use tokio::sync::mpsc::error::TrySendError;

pub enum ResumeError<Key, Err, Tx, Rx>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    HandshakeIncomplete(SessionIncomplete<Tx, Rx>),
    HandshakeError {
        error: Err,
        tx: Result<Tx, IncompleteHalf<Tx>>,
        rx: Result<Rx, IncompleteHalf<Rx>>,
    },
    NoSuchKey {
        key: Key,
        tx: Tx,
        rx: Rx,
    },
    KeyAlreadyExists {
        key: Key,
        tx: Tx,
        rx: Rx,
    },
    ResumeIncomplete(ResumeIncomplete<Tx, Rx>),
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<Key, Err, Tx, Rx> ResumeError<Key, Err, Tx, Rx> {
    pub fn into_halves(self) -> (Option<Tx>, Option<Rx>) {
        use ResumeError::*;

        fn unwrap_incomplete<T>(result: Result<T, IncompleteHalf<T>>) -> Option<T> {
            match result {
                Ok(t) => Some(t),
                Err(IncompleteHalf::Unfinished(t)) => Some(t),
                Err(IncompleteHalf::Unclosed) => None,
            }
        }

        match self {
            HandshakeIncomplete(incomplete) => {
                let (tx, rx) = incomplete.into_halves();
                (unwrap_incomplete(tx), unwrap_incomplete(rx))
            }
            HandshakeError { tx, rx, .. } => (unwrap_incomplete(tx), unwrap_incomplete(rx)),
            NoSuchKey { tx, rx, .. } => (Some(tx), Some(rx)),
            KeyAlreadyExists { tx, rx, .. } => (Some(tx), Some(rx)),
            ResumeIncomplete(incomplete) => incomplete.into_halves(),
        }
    }
}

pub enum ResumeIncomplete<Tx, Rx> {
    BothHalves {
        tx_error: TrySendError<Tx>,
        rx_error: TrySendError<Rx>,
    },
    TxHalf {
        tx_error: TrySendError<Tx>,
    },
    RxHalf {
        rx_error: TrySendError<Rx>,
    },
}

impl<Tx, Rx> ResumeIncomplete<Tx, Rx> {
    pub fn into_halves(self) -> (Option<Tx>, Option<Rx>) {
        use ResumeIncomplete::*;

        fn unwrap<T>(e: TrySendError<T>) -> T {
            match e {
                TrySendError::Full(t) => t,
                TrySendError::Closed(t) => t,
            }
        }

        match self {
            BothHalves { tx_error, rx_error } => (Some(unwrap(tx_error)), Some(unwrap(rx_error))),
            TxHalf { tx_error } => (Some(unwrap(tx_error)), None),
            RxHalf { rx_error } => (None, Some(unwrap(rx_error))),
        }
    }
}
