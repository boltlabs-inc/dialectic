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
    retry::{ConnectTo, Init, Recovery, Retry, RetryError},
    util::{sleep_until_or_deadline, timeout_at_option},
};

/// The insides of the [`Sender`] and [`Receiver`] are almost identical, and the functionality we
/// need is almost the same. The `End` struct is the insides of both `Sender` and `Receiver`: all
/// that  differs is the choice of type parameter.
///
/// Ends come in linked pairs of sender/receiver, and each pair shares three mutexes: `key`,
/// `next_inner`, and `next_other`. The `next_inner` of one half is the `next_other` of the other,
/// and vice-versa. When either end encounters an error it needs to recover from, it tries to pull
/// from the `next_inner` queue. If no next inner is available, it reconnects, thereby generating
/// *both* a new `Tx` and `Rx`, and pushes the one which is not its own side onto the `next_other`
/// queue. To avoid a reference cycle, `Weak` references are used for `next_other`.
///
/// There is an opportunity for a memory leak if one half of the pair is not used for an unbounded
/// amount of time (i.e. in a loop where things are only sent, not received). Every error that
/// occurs will result in another item being added to the queue of the other half, and the queue
/// will only be popped-from when that half is next used. This is unavoidable, because we cannot
/// know whether there is important information still to-be-sent or to-be-received in the buffer of
/// the underlying `Tx` or `Rx`, so we cannot pre-emptively throw it away until the error is
/// encountered by the other side.
#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub(super) struct End<H: Session, Address, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
{
    /// The address to connect to.
    address: Arc<Address>,
    /// The key for this session, if one has been assigned.
    key: Arc<Mutex<Option<Key>>>,
    /// The current inner thing (a `tx` or `rx` connection end).
    inner: Option<Inner>,
    /// A queue of next inner things, from which we will pop when necessary.
    next_inner: Arc<Mutex<VecDeque<Inner>>>,
    /// A queue of next other things, to which we will push when we create a new connection.
    next_other: Weak<Mutex<VecDeque<Other>>>,
    //
    // ---------- Reconnect strategy closures below this point ----------
    //
    /// An async function to create a new connection, or return an error.
    #[derivative(Debug = "ignore")]
    connect: Arc<ConnectTo<Address, ConnectErr, Tx, Rx>>,
    /// An async function to perform an initial handshake.
    #[derivative(Debug = "ignore")]
    init: Arc<Init<H, Key, HandshakeErr, Tx, Rx>>,
    /// An async function to perform an initial handshake.
    #[derivative(Debug = "ignore")]
    retry: Arc<Retry<H, Key, HandshakeErr, Tx, Rx>>,
    /// A function describing the desired retry strategy for errors in the underlying connection.
    #[derivative(Debug = "ignore")]
    recover: Arc<dyn Fn(usize, &Err) -> Recovery + Sync + Send>,
    /// A function describing the desired retry strategy for errors while attempting to establish a
    /// connection.
    #[derivative(Debug = "ignore")]
    recover_connect: Arc<dyn Fn(usize, &ConnectErr) -> Recovery + Sync + Send>,
    /// A function describing the desired retry strategy for errors while attempting to perform a
    /// handshake.
    #[derivative(Debug = "ignore")]
    recover_handshake: Arc<dyn Fn(usize, &HandshakeErr) -> Recovery + Sync + Send>,
    /// An optional timeout, which bounds all retry attempts.
    timeout: Option<Duration>,
    /// The maximum size for the queues.
    max_pending_retries: usize,
}

/// A sending retry end is an [`End`] whose `Inner` is `Tx` and whose `Other` is `Rx`k.
pub(super) type SenderEnd<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx> =
    End<H, Address, Key, <Tx as Transmitter>::Error, ConnectErr, HandshakeErr, Tx, Rx, Tx, Rx>;

/// A receiving retry end is an [`End`] whose `Inner` is `Rx` and whose `Other` is `Tx`.
pub(super) type ReceiverEnd<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx> =
    End<H, Address, Key, <Rx as Receiver>::Error, ConnectErr, HandshakeErr, Rx, Tx, Tx, Rx>;

/// A set of the four recovery strategies necessary to specify an `End`'s strategy.
#[Transmitter(Tx)]
#[Receiver(Rx)]
#[derive(Clone)]
pub(super) struct Recoveries<ConnectErr, HandshakeErr, Tx, Rx> {
    pub connect: Arc<dyn Fn(usize, &ConnectErr) -> Recovery + Sync + Send>,
    pub handshake: Arc<dyn Fn(usize, &HandshakeErr) -> Recovery + Sync + Send>,
    pub tx: Arc<dyn Fn(usize, &Tx::Error) -> Recovery + Sync + Send>,
    pub rx: Arc<dyn Fn(usize, &Rx::Error) -> Recovery + Sync + Send>,
}

/// Create a [`SenderEnd`]/[`ReceiverEnd`] linked pair. This *does not* actually connect yet to the
/// specified address.
#[Transmitter(Tx)]
#[Receiver(Rx)]
#[allow(clippy::type_complexity)]
pub(super) fn channel<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>(
    address: Address,
    connect: Arc<ConnectTo<Address, ConnectErr, Tx, Rx>>,
    init: Arc<Init<H, Key, HandshakeErr, Tx, Rx>>,
    retry: Arc<Retry<H, Key, HandshakeErr, Tx, Rx>>,
    recoveries: Recoveries<ConnectErr, HandshakeErr, Tx, Rx>,
    timeout: Option<Duration>,
    max_pending_retries: usize,
) -> (
    SenderEnd<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>,
    ReceiverEnd<H, Address, Key, ConnectErr, HandshakeErr, Tx, Rx>,
)
where
    H: Session,
    Key: Sync + Send + Clone + 'static,
    ConnectErr: 'static,
    HandshakeErr: 'static,
{
    // The shared state for both sides
    let address = Arc::new(address);
    let key = Arc::new(Mutex::new(None));
    let next_tx = Arc::new(Mutex::new(VecDeque::new()));
    let next_rx = Arc::new(Mutex::new(VecDeque::new()));

    let sender = End {
        address: address.clone(),
        key: key.clone(),
        inner: None,
        next_inner: next_tx.clone(),
        next_other: Arc::downgrade(&next_rx),
        init: init.clone(),
        retry: retry.clone(),
        connect: connect.clone(),
        recover: recoveries.tx,
        recover_connect: recoveries.connect.clone(),
        recover_handshake: recoveries.handshake.clone(),
        timeout,
        max_pending_retries,
    };

    let receiver = End {
        address,
        key,
        inner: None,
        next_inner: next_rx,
        next_other: Arc::downgrade(&next_tx),
        init,
        retry,
        connect,
        recover: recoveries.rx,
        recover_connect: recoveries.connect,
        recover_handshake: recoveries.handshake,
        timeout,
        max_pending_retries,
    };

    (sender, receiver)
}

#[Transmitter(Tx)]
#[Receiver(Rx)]
impl<H: Session, Address, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
    End<H, Address, Key, Err, ConnectErr, HandshakeErr, Inner, Other, Tx, Rx>
{
    /// Given an error, interpret the contained recovery strategy and return `true` if and only if
    /// we should continue onwards. This function modifies `retries` and `deadline` to keep them
    /// updated, with the assumption that they are initialized on the first occurrence of this error
    /// at `0` and `None`, respectively.
    #[inline(always)]
    pub(super) async fn recover(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
        error: &Err,
    ) -> bool {
        // Set the deadline if it hasn't been set already
        if *retries == 0 && deadline.is_none() {
            *deadline = self.timeout.map(|delay| Instant::now() + delay);
        }

        // Determine the error recovery strategy
        let strategy = (self.recover)(*retries, error);

        // Sleep until the correct deadline, or return an error if we would exceed the deadline or
        // the strategy is `Fail`
        match strategy {
            Recovery::ReconnectAfter(after) => {
                self.disconnect();
                if !sleep_until_or_deadline(after, *deadline).await {
                    return false;
                }
            }
            Recovery::Fail => return false,
        }

        // Increment retries, because we just did a recovery
        *retries += 1;

        // We should continue now
        true
    }

    /// Disconnect the inner connection of this `End`, so that future calls will require a
    /// reconnection before anything happens.
    #[inline(always)]
    fn disconnect(&mut self) {
        self.inner = None;
    }

    /// Initialize the connection if necessary, and return the session key.
    pub(super) async fn key<E>(
        &mut self,
        reorder: impl Fn(Tx, Rx) -> (Inner, Other),
    ) -> Result<Key, RetryError<E, ConnectErr, HandshakeErr>>
    where
        Address: Clone,
        Key: Clone,
    {
        if self.key.lock().await.is_none() {
            let _ = self.inner(reorder, &mut 0, &mut None).await?;
        }
        let key = self.key.lock().await.clone().unwrap();
        Ok(key)
    }

    /// Mutably borrow the inner connection of this `End`, recreating the connection if necessary,
    /// or returning an error if it's not possible to do so within the bounds described by the
    /// reconnection strategies.
    #[inline(always)]
    pub(super) async fn inner<E>(
        &mut self,
        reorder: impl Fn(Tx, Rx) -> (Inner, Other),
        retries: &mut usize,
        deadline: &mut Option<Instant>,
    ) -> Result<&mut Inner, RetryError<E, ConnectErr, HandshakeErr>>
    where
        Key: Clone,
        Address: Clone,
    {
        // If there is no `inner` end currently in its slot, we need to acquire one
        if self.inner.is_none() {
            self.inner = Some(loop {
                // Take out a lock on the key (must come first to avoid deadlock)
                // Even though it wouldn't result in deadlock to do this slightly later, taking the
                // lock here means that we never try to concurrently connect!
                let mut key = self.key.lock().await;

                // Lock the next inner queue, so nobody else will push to it during this operation
                let mut next_inner = self.next_inner.lock().await;

                if let Some(inner) = next_inner.pop_front() {
                    // There was a waiting `inner` for us in the queue, so set it, shrinking the
                    // queue if it's below 25% capacity
                    if next_inner.len().saturating_mul(4) < next_inner.capacity() {
                        next_inner.shrink_to_fit();
                    }
                    break inner;
                } else {
                    // Try to reconnect, exiting the loop if successful, collecting the error if not
                    let (strategy, err) =
                        match timeout_at_option(*deadline, (self.connect)((*self.address).clone()))
                            .await
                        {
                            // Deadline exceeded while waiting for reconnect to finish
                            Err(_) => return Err(RetryError::ConnectTimeout),
                            // Reconnect returned some error, so we should try to recover from that
                            // error and try again
                            Ok(Err(err)) => (
                                (self.recover_connect)(*retries, &err),
                                RetryError::ConnectError(err),
                            ),
                            // Reconnect successfully returned a connection for us to do a handshake
                            Ok(Ok((tx, rx))) => {
                                // Execute the handshake
                                let handshake = H::over(tx, rx, |chan| async {
                                    match *key {
                                        // Initial handshake
                                        None => {
                                            let new_key = (self.init)(chan).await?;
                                            // Set the key
                                            *key = Some(new_key);
                                            Ok(())
                                        }
                                        // Subsequent handshake
                                        Some(ref key) => (self.retry)(key.clone(), chan).await,
                                    }
                                });
                                match timeout_at_option(*deadline, handshake).await {
                                    // Deadline exceeded while waiting for handshake to finish
                                    Err(_) => return Err(RetryError::HandshakeTimeout),
                                    // Handshake session was incomplete
                                    Ok((Ok(_), Err(_))) => {
                                        return Err(RetryError::HandshakeIncomplete)
                                    }
                                    // Handshake session returned an error, from which we should try
                                    // to recover and try again
                                    Ok((Err(err), _)) => (
                                        (self.recover_handshake)(*retries, &err),
                                        RetryError::HandshakeError(err),
                                    ),
                                    // Handshake session succeeded
                                    Ok((Ok(()), Ok((tx, rx)))) => {
                                        // Depending on whether we're in a `Receiver` or `Sender`,
                                        // swap the tx/rx for each other to get an inner/other pair
                                        let (inner, other) = reorder(tx, rx);
                                        // Put the new `other` on the other's queue
                                        if let Some(mut q) = lock_weak(&self.next_other).await {
                                            // If we would exceed the capacity of the other's queue,
                                            // return an error instead of pushing onto it
                                            if q.len() > self.max_pending_retries {
                                                return Err(RetryError::NoCapacity);
                                            } else {
                                                q.push_back(other);
                                            }
                                        }
                                        // Set the new `inner`
                                        break inner;
                                    }
                                }
                            }
                        };

                    // Drop the lock on key (must come last to avoid deadlock)
                    drop(key);

                    // Set the deadline after the first error occurs
                    if *retries == 0 && deadline.is_none() {
                        *deadline = self.timeout.map(|delay| Instant::now() + delay);
                    }

                    // Depending on the strategy, either try again or bail with the error
                    match strategy {
                        Recovery::Fail => return Err(err),
                        Recovery::ReconnectAfter(after) => {
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
