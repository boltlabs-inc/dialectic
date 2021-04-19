use dialectic::{
    backend::{self, By},
    prelude::*,
    SessionIncomplete,
};
use std::{
    collections::VecDeque,
    future::Future,
    hash::Hash,
    marker::PhantomData,
    pin::Pin,
    sync::{Arc, Weak},
    time::Duration,
};
use tokio::{
    pin, select,
    sync::{Mutex, OwnedMutexGuard},
    time::Instant,
};

use crate::ConnectKind;

pub enum RetryStrategy {
    RetryAfter(Duration),
    ReconnectAfter(Duration),
    Fail,
}

pub enum ReconnectStrategy {
    ReconnectAfter(Duration),
    Fail,
}

type Handshake<H, Key, Err, Tx, Rx> = dyn Fn(Option<&Key>, Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<Key, Err>>>>
    + Send
    + Sync;

type Connect<Err, Tx, Rx> =
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<(Tx, Rx), Err>>>> + Send + Sync;

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub async fn channel<H: Session, S: Session, Key, Err, ConnectFut, HandshakeFut, Tx, Rx>(
    connect: impl Fn() -> ConnectFut + Sync + Send + 'static,
    handshake: impl Fn(Option<&Key>, Chan<H, Tx, Rx>) -> HandshakeFut + Sync + Send + 'static,
    recover_connect: impl Fn(usize, &Err) -> ReconnectStrategy + Sync + Send + 'static,
    recover_tx: impl Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send + 'static,
    recover_rx: impl Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send + 'static,
    timeout: Option<Duration>,
) -> Chan<S, Sender<H, Key, Err, Tx, Rx>, Receiver<H, Key, Err, Tx, Rx>>
where
    Key: Sync + Send + Clone + 'static,
    ConnectFut: Future<Output = Result<(Tx, Rx), Err>> + 'static,
    HandshakeFut: Future<Output = Result<Key, Err>> + 'static,
    Err: 'static,
{
    // The shared closures for both sides
    let handshake: Arc<Handshake<H, Key, Err, Tx, Rx>> =
        Arc::new(move |key, chan| Box::pin(handshake(key, chan)));
    let connect: Arc<Connect<Err, Tx, Rx>> = Arc::new(move || Box::pin(connect()));
    let recover_connect = Arc::new(recover_connect);

    // The shared mutable state for both sides
    let key = Arc::new(Mutex::new(None));
    let next_tx = Arc::new(Mutex::new(VecDeque::new()));
    let next_rx = Arc::new(Mutex::new(VecDeque::new()));

    let sender = Sender {
        key: key.clone(),
        tx: None,
        next_tx: next_tx.clone(),
        next_rx: Arc::downgrade(&next_rx),
        handshake: handshake.clone(),
        connect: connect.clone(),
        recover_tx: Box::new(recover_tx),
        recover_connect: recover_connect.clone(),
        timeout,
    };

    let receiver = Receiver {
        key,
        rx: None,
        next_tx: Arc::downgrade(&next_tx),
        next_rx,
        handshake,
        connect,
        recover_rx: Box::new(recover_rx),
        recover_connect,
        timeout,
    };

    S::wrap(sender, receiver)
}

async fn lock_owned_weak<T>(weak: &Weak<Mutex<T>>) -> Option<OwnedMutexGuard<T>> {
    if let Some(arc) = weak.upgrade() {
        Some(arc.lock_owned().await)
    } else {
        None
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Sender<H: Session, Key, Err, Tx, Rx> {
    key: Arc<Mutex<Option<Key>>>,
    tx: Option<Tx>,
    next_tx: Arc<Mutex<VecDeque<Tx>>>,  // strong reference to `Tx`
    next_rx: Weak<Mutex<VecDeque<Rx>>>, // weak reference to `Rx` (disappears if `Receiver` drops)
    handshake: Arc<Handshake<H, Key, Err, Tx, Rx>>,
    connect: Arc<Connect<Err, Tx, Rx>>,
    recover_tx: Box<dyn Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send + 'static>,
    recover_connect: Arc<dyn Fn(usize, &Err) -> ReconnectStrategy + Sync + Send + 'static>,
    timeout: Option<Duration>,
}

enum RetryError<Err> {
    ConnectError(Err),
    HandshakeIncomplete,
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, Err, Tx, Rx> Sender<H, Key, Err, Tx, Rx> {
    async fn tx(&mut self) -> Result<&mut Tx, RetryError<Err>> {
        // If there is no `tx` end currently in its slot, we need to acquire one
        if self.tx.is_none() {
            let mut retries = 0;
            let mut deadline = None;

            self.tx = Some(loop {
                if let Some(tx) = self.next_tx.lock().await.pop_front() {
                    // There was a waiting `tx` for us in the queue, so set it
                    break tx;
                } else {
                    // Try to reconnect, exiting the loop if successful, collecting the error if not
                    let err: Err = match (self.connect)().await {
                        Err(err) => err,
                        Ok((tx, rx)) => {
                            // Take out a lock on the key (must come first to avoid deadlock)
                            let mut key = self.key.lock().await;

                            // Execute the handshake
                            let (result, ends) =
                                H::over(tx, rx, |chan| (self.handshake)(key.as_ref(), chan)).await;

                            match (result, ends) {
                                // Handshake session returned an error
                                (Err(err), _) => err,
                                // Handshake session was incomplete
                                (Ok(_), Err(_)) => return Err(RetryError::HandshakeIncomplete),
                                // Handshake session succeeded
                                (Ok(new_key), Ok((tx, rx))) => {
                                    // Put the new `rx` on the `Receiver`'s queue
                                    if let Some(mut q) = lock_owned_weak(&self.next_rx).await {
                                        q.push_back(rx);
                                    }
                                    // Set the new key (must come last to avoid deadlock)
                                    *key = Some(new_key);
                                    // Set the new `tx`
                                    break tx;
                                }
                            }
                        }
                    };

                    // Set the deadline after the first error occurs
                    if retries == 0 {
                        deadline = self.timeout.map(|delay| Instant::now() + delay);
                    }

                    // Recover from this error, based on the strategy given by `recover_connect`
                    match (self.recover_connect)(retries, &err) {
                        ReconnectStrategy::Fail => return Err(RetryError::ConnectError(err)),
                        ReconnectStrategy::ReconnectAfter(after) => {
                            let wakeup = Instant::now() + after;
                            if let Some(deadline) = deadline {
                                // If we would exceed the deadline, don't wait at all
                                if deadline < wakeup {
                                    return Err(RetryError::ConnectError(err));
                                }
                            }
                            tokio::time::sleep_until(wakeup).await;
                        }
                    }
                };

                // Increase the count, so next iteration we can recover differently
                retries += 1;
            });
        }

        // No matter what, if execution reaches this point, `self.tx.is_some()` will hold
        Ok(self.tx.as_mut().unwrap())
    }
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct Receiver<H: Session, Key, Err, Tx, Rx> {
    key: Arc<Mutex<Option<Key>>>,
    rx: Option<Rx>,
    next_tx: Weak<Mutex<VecDeque<Tx>>>, // weak reference to `Tx` (disappears if `Sender` drops)
    next_rx: Arc<Mutex<VecDeque<Rx>>>,  // strong reference to `Rx`
    handshake: Arc<Handshake<H, Key, Err, Tx, Rx>>,
    connect: Arc<Connect<Err, Tx, Rx>>,
    recover_rx: Box<dyn Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send + 'static>,
    recover_connect: Arc<dyn Fn(usize, &Err) -> ReconnectStrategy + Sync + Send + 'static>,
    timeout: Option<Duration>,
}
