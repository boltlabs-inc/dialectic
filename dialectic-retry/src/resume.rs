use async_trait::async_trait;
use dashmap::DashMap;
use dialectic::{backend, prelude::*, IncompleteHalf, SessionIncomplete};
use futures::{ready, stream::FuturesUnordered};
use pin_project::pin_project;
use std::{
    future::Future,
    hash::Hash,
    marker::PhantomData,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Weak,
    },
    task::{Context, Poll},
    time::{Duration, Instant},
};
use tokio::{
    sync::mpsc::{self, error::TrySendError},
    time::Sleep,
};

#[async_trait]
pub trait Resume<Key, Tx, Rx>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    type Session: Session;
    type Error;

    async fn handshake(&self, chan: Chan<Self::Session, Tx, Rx>) -> Result<Key, Self::Error>;
}

pub enum ErrorStrategy {
    Retry(Duration),
    RetryAfterReconnect,
}

type Managed<Key, Tx, Rx> = DashMap<Key, (mpsc::Sender<Tx>, mpsc::Sender<Rx>)>;

type RecoverTx<Tx> =
    fn(&<Tx as backend::Transmitter>::Error) -> Box<dyn Iterator<Item = ErrorStrategy>>;

type RecoverRx<Rx> =
    fn(&<Rx as backend::Receiver>::Error) -> Box<dyn Iterator<Item = ErrorStrategy>>;

#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Clone)]
pub struct Resumer<R, Key, S, Tx, Rx> {
    resume: R,
    queue_size: usize,
    managed: Arc<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
    recover_tx: RecoverTx<Tx>,
    recover_rx: RecoverRx<Rx>,
    session: PhantomData<S>,
}

pub enum ResumeError<R, Key, Tx, Rx>
where
    R: Resume<Key, Tx, Rx>,
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    HandshakeIncomplete(SessionIncomplete<Tx, Rx>),
    HandshakeError {
        error: R::Error,
        tx: Result<Tx, IncompleteHalf<Tx>>,
        rx: Result<Rx, IncompleteHalf<Rx>>,
    },
    ResumeIncomplete(ResumeIncomplete<Tx, Rx>),
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

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<R, Key, S, Tx, Rx> Resumer<R, Key, S, Tx, Rx>
where
    R: Resume<Key, Tx, Rx>,
    Key: Clone + Eq + Hash + Send + Sync + 'static,
    S: Session,
{
    pub async fn resume(
        &self,
        tx: Tx,
        rx: Rx,
    ) -> Result<
        Option<Chan<S, Sender<Key, Tx, Rx>, Receiver<Key, Tx, Rx>>>,
        ResumeError<R, Key, Tx, Rx>,
    > {
        let (result, ends) = <R::Session>::over(tx, rx, |chan| self.resume.handshake(chan)).await;
        let key = match result {
            Ok(key) => key,
            Err(error) => {
                let (tx, rx) = ends
                    .map(|(tx, rx)| (Ok(tx), Ok(rx)))
                    .unwrap_or_else(SessionIncomplete::into_halves);
                return Err(ResumeError::HandshakeError { error, tx, rx });
            }
        };
        match ends {
            Ok((tx, rx)) => {
                if let Some(map_ref) = self.managed.get(&key) {
                    // If there was already an entry by this key, there's a session, potentially
                    // paused, by this key. If the handshake was properly implemented, we're only
                    // receiving this tx/rx pair because the other end tried to reconnect, which
                    // means that the connection broke in one direction or another, so either we are
                    // currently paused, or we're about to discover the issue and pause. Sending the
                    // tx/rx will un-pause us and allow us to resume on those fresh channels.
                    let send_tx = &map_ref.0;
                    let send_rx = &map_ref.1;
                    match (send_tx.try_send(tx), send_rx.try_send(rx)) {
                        (Ok(()), Ok(())) => Ok(None),
                        (Err(tx_error), Err(rx_error)) => Err(ResumeError::ResumeIncomplete(
                            ResumeIncomplete::BothHalves { tx_error, rx_error },
                        )),
                        (Err(tx_error), Ok(())) => {
                            Err(ResumeError::ResumeIncomplete(ResumeIncomplete::TxHalf {
                                tx_error,
                            }))
                        }
                        (Ok(()), Err(rx_error)) => {
                            Err(ResumeError::ResumeIncomplete(ResumeIncomplete::RxHalf {
                                rx_error,
                            }))
                        }
                    }
                } else {
                    // If there was no entry by this key, then this represents a fresh session, and
                    // we should return a channel for the caller to do with what they like (usually,
                    // run a session in a separate task).
                    let (send_next_tx, next_tx) = mpsc::channel(self.queue_size);
                    let (send_next_rx, next_rx) = mpsc::channel(self.queue_size);
                    self.managed
                        .insert(key.clone(), (send_next_tx, send_next_rx));
                    let tx = Sender {
                        key: key.clone(),
                        tx,
                        next_tx,
                        recover_tx: self.recover_tx,
                        managed: Arc::downgrade(&self.managed),
                        timeout: self.timeout,
                    };
                    let rx = Receiver {
                        key,
                        rx,
                        next_rx,
                        recover_rx: self.recover_rx,
                        managed: Arc::downgrade(&self.managed),
                        timeout: self.timeout,
                    };
                    Ok(Some(S::wrap(tx, rx)))
                }
            }
            Err(incomplete) => Err(ResumeError::HandshakeIncomplete(incomplete)),
        }
    }
}

#[Transmitter(Tx)]
pub struct Sender<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    key: Key,
    tx: Tx,
    next_tx: mpsc::Receiver<Tx>,
    recover_tx: RecoverTx<Tx>,
    managed: Weak<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
}

#[Transmitter(Tx)]
impl<Key, Tx, Rx> Drop for Sender<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    fn drop(&mut self) {
        // Remove the entire entry at this key if the other end has already dropped
        self.managed
            .upgrade()
            .map(|arc| arc.remove_if(&self.key, |_, (_, send_rx)| send_rx.is_closed()));
    }
}

#[Receiver(Rx)]
pub struct Receiver<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    key: Key,
    rx: Rx,
    next_rx: mpsc::Receiver<Rx>,
    recover_rx: RecoverRx<Rx>,
    managed: Weak<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
}

#[Receiver(Rx)]
impl<Key, Tx, Rx> Drop for Receiver<Key, Tx, Rx>
where
    Key: Eq + Hash,
{
    fn drop(&mut self) {
        // Remove the entire entry at this key if the other end has already dropped
        self.managed
            .upgrade()
            .map(|arc| arc.remove_if(&self.key, |_, (send_tx, _)| send_tx.is_closed()));
    }
}
