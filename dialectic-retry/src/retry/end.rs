use dialectic::prelude::*;
use std::{
    collections::VecDeque,
    sync::{Arc, Weak},
    time::Duration,
};
use tokio::{
    sync::{Mutex, OwnedMutexGuard},
    time::Instant,
};

use crate::{
    retry::{Connect, Handshake, ReconnectStrategy, RetryError, RetryStrategy},
    util::{sleep_until_or_deadline, timeout_at_option},
};

/// The insides of the [`Sender`] and [`Receiver`] are almost identical, and the functionality we
/// need is almost the same. The `End` struct is the insides of both `Sender` and `Receiver`: all
/// that  differs is the choice of type parameter.
#[Transmitter(Tx)]
#[Receiver(Rx)]
pub struct End<H: Session, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx> {
    /// The current key for this session, if one has been assigned.
    key: Arc<Mutex<Option<Key>>>,
    /// The current inner thing (a `tx` or `rx` connection end).
    inner: Option<Inner>,
    /// A queue of next inner things, from which we will pop when necessary.
    next_inner: Arc<Mutex<VecDeque<Inner>>>,
    /// A queue of next other things, to which we will push when we create a new connection.
    next_other: Weak<Mutex<VecDeque<Other>>>,
    /// An async function to create a new connection, or return an error.
    connect: Arc<Connect<ConnectErr, Tx, Rx>>,
    /// An async function to perform a handshake, starting with an optional key and returning a new
    /// key or an error.
    handshake: Arc<Handshake<H, Key, HandshakeErr, Tx, Rx>>,
    /// A function describing the desired retry strategy for errors in the underlying connection.
    recover: Arc<dyn Fn(usize, &Err) -> RetryStrategy + Sync + Send>,
    /// A function describing the desired retry strategy for errors while attempting to establish a
    /// connection.
    recover_connect: Arc<dyn Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send>,
    /// A function describing the desired retry strategy for errors while attempting to perform a
    /// handshake.
    recover_handshake: Arc<dyn Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send>,
    /// An optional timeout, which bounds all retry attempts.
    timeout: Option<Duration>,
}

pub type SenderEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx> =
    End<H, Key, <Tx as Transmitter>::Error, ConnectErr, HandshakeErr, Tx, Rx, Tx, Rx>;

pub type ReceiverEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx> =
    End<H, Key, <Rx as Receiver>::Error, ConnectErr, HandshakeErr, Rx, Tx, Tx, Rx>;

#[Transmitter(Tx)]
#[Receiver(Rx)]
#[allow(clippy::type_complexity)]
pub fn channel<H, Key, ConnectErr, HandshakeErr, Tx, Rx>(
    connect: Arc<Connect<ConnectErr, Tx, Rx>>,
    handshake: Arc<Handshake<H, Key, HandshakeErr, Tx, Rx>>,
    recover_connect: Arc<dyn Fn(usize, &ConnectErr) -> ReconnectStrategy + Sync + Send>,
    recover_handshake: Arc<dyn Fn(usize, &HandshakeErr) -> ReconnectStrategy + Sync + Send>,
    recover_tx: Arc<dyn Fn(usize, &Tx::Error) -> RetryStrategy + Sync + Send>,
    recover_rx: Arc<dyn Fn(usize, &Rx::Error) -> RetryStrategy + Sync + Send>,
    timeout: Option<Duration>,
) -> (
    SenderEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
    ReceiverEnd<H, Key, ConnectErr, HandshakeErr, Tx, Rx>,
)
where
    H: Session,
    Key: Sync + Send + Clone + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    // The shared mutable state for both sides
    let key = Arc::new(Mutex::new(None));
    let next_tx = Arc::new(Mutex::new(VecDeque::new()));
    let next_rx = Arc::new(Mutex::new(VecDeque::new()));

    let sender = End {
        key: key.clone(),
        inner: None,
        next_inner: next_tx.clone(),
        next_other: Arc::downgrade(&next_rx),
        handshake: handshake.clone(),
        connect: connect.clone(),
        recover: recover_tx,
        recover_connect: recover_connect.clone(),
        recover_handshake: recover_handshake.clone(),
        timeout,
    };

    let receiver = End {
        key,
        inner: None,
        next_inner: next_rx,
        next_other: Arc::downgrade(&next_tx),
        handshake,
        connect,
        recover: recover_rx,
        recover_connect,
        recover_handshake,
        timeout,
    };

    (sender, receiver)
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
    End<H, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
{
    /// Try to recover from a given error according to the recovery strategy specified in this
    /// `End`, otherwise returning the same error if the strategy times out or reconnection is
    /// unsuccessful.
    #[inline(always)]
    pub async fn recover(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
        error: Err,
    ) -> Result<(), RetryError<Err, ConnectErr, HandshakeErr>> {
        // Set the deadline if it hasn't been set already
        if *retries == 0 && deadline.is_none() {
            *deadline = self.timeout.map(|delay| Instant::now() + delay);
        }

        // Determine the error recovery strategy
        let strategy = (self.recover)(*retries, &error);

        // Drop the current `inner` if we're about to reconnect
        if matches!(strategy, RetryStrategy::ReconnectAfter(_)) {
            self.disconnect();
        }

        // Sleep until the correct deadline, or return an error if we would exceed the deadline or
        // the strategy is `Fail`
        match strategy {
            RetryStrategy::RetryAfter(after) | RetryStrategy::ReconnectAfter(after) => {
                if !sleep_until_or_deadline(after, *deadline).await {
                    return Err(RetryError::OriginalError(error));
                }
            }
            RetryStrategy::Fail => return Err(RetryError::OriginalError(error)),
        }

        // Increment retries, because we just did a recovery
        *retries += 1;

        Ok(())
    }

    /// Disconnect the inner connection of this `End`, so that future calls will require a
    /// reconnection before anything happens.
    #[inline(always)]
    fn disconnect(&mut self) {
        self.inner = None;
    }

    /// Mutably borrow the inner connection of this `End`, recreating the connection if necessary,
    /// or returning an error if it's not possible to do so within the bounds described by the
    /// reconnection strategies.
    #[inline(always)]
    pub async fn inner<E>(
        &mut self,
        reorder: impl Fn(Tx, Rx) -> (Inner, Other),
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Inner, RetryError<E, ConnectErr, HandshakeErr>> {
        // If there is no `inner` end currently in its slot, we need to acquire one
        if self.inner.is_none() {
            self.inner = Some(loop {
                // Take out a lock on the key (must come first to avoid deadlock)
                // Even though it wouldn't result in deadlock to do this slightly later, taking the
                // lock here means that we never try to concurrently connect!
                let mut key = self.key.lock().await;

                if let Some(inner) = self.next_inner.lock().await.pop_front() {
                    // There was a waiting `inner` for us in the queue, so set it
                    break inner;
                } else {
                    // Try to reconnect, exiting the loop if successful, collecting the error if not
                    let (strategy, err) = match timeout_at_option(*deadline, (self.connect)()).await
                    {
                        // Deadline exceeded while waiting for reconnect to finish
                        Err(_) => return Err(RetryError::ConnectTimeout),
                        // Reconnect returned some error, so we should try to recover from that
                        // error and try again
                        Ok(Err(err)) => (
                            (self.recover_connect)(*retries, &err),
                            RetryError::ConnectError(err),
                        ),
                        // Reconnect successfully returned a connection for us to do a handshake on
                        Ok(Ok((tx, rx))) => {
                            // Execute the handshake
                            let handshake =
                                H::over(tx, rx, |chan| (self.handshake)(key.as_ref(), chan));
                            match timeout_at_option(*deadline, handshake).await {
                                // Deadline exceeded while waiting for handshake to finish
                                Err(_) => return Err(RetryError::HandshakeTimeout),
                                // Handshake session was incomplete
                                Ok((Ok(_), Err(_))) => return Err(RetryError::HandshakeIncomplete),
                                // Handshake session returned an error, from which we should try to
                                // recover and try again
                                Ok((Err(err), _)) => (
                                    (self.recover_handshake)(*retries, &err),
                                    RetryError::HandshakeError(err),
                                ),
                                // Handshake session succeeded
                                Ok((Ok(new_key), Ok((tx, rx)))) => {
                                    // Depending on whether we're in a `Receiver` or `Sender`, swap
                                    // the tx/rx for each other to get an inner/other pair
                                    let (inner, other) = reorder(tx, rx);
                                    // Put the new `other` on the other's queue
                                    if let Some(mut q) = lock_weak(&self.next_other).await {
                                        q.push_back(other);
                                    }
                                    // Set the new key (must come last to avoid deadlock)
                                    *key = Some(new_key);
                                    // Set the new `inner`
                                    break inner;
                                }
                            }
                        }
                    };

                    // Set the deadline after the first error occurs
                    if *retries == 0 && deadline.is_none() {
                        *deadline = self.timeout.map(|delay| Instant::now() + delay);
                    }

                    // Depending on the strategy, either try again or bail with the error
                    match strategy {
                        ReconnectStrategy::Fail => return Err(err),
                        ReconnectStrategy::ReconnectAfter(after) => {
                            if !sleep_until_or_deadline(after, *deadline).await {
                                return Err(err);
                            }
                        }
                    }
                };

                // Increase the count, so next iteration we can recover differently
                *retries += 1;
            });
        }

        // No matter what, if execution reaches this point, `self.inner.is_some()` will hold
        Ok(self.inner.as_mut().unwrap())
    }
}

/// Attempt to lock a mutex behind a weak pointer, returning `None` if its corresponding `Arc` no
/// longer exists.
async fn lock_weak<T>(weak: &Weak<Mutex<T>>) -> Option<OwnedMutexGuard<T>> {
    if let Some(arc) = weak.upgrade() {
        Some(arc.lock_owned().await)
    } else {
        None
    }
}
