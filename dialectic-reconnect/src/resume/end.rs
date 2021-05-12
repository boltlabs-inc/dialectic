use dialectic::backend;
use std::{
    hash::Hash,
    sync::{atomic::Ordering, Arc},
    time::Duration,
};
use tokio::time::Instant;

use super::{Managed, Recovery};
use crate::maybe_bounded;
use crate::util::timeout_at_option;

/// Both sending and receiving ends have shared recovery logic; this struct is used for both ends.
#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub(super) struct End<Key, Err, Inner, Tx, Rx>
where
    Key: Eq + Hash,
{
    key: Key,
    inner: Option<Inner>,
    next: maybe_bounded::Receiver<Inner>,
    #[derivative(Debug = "ignore")]
    recover: Arc<dyn Fn(usize, &Err) -> Recovery + Sync + Send>,
    managed: Arc<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
}

/// A resuming sender end is an [`End`] whose `Inner` is `Tx` and whose `Error` is a `Tx::Error`.
pub(super) type SenderEnd<Key, Tx, Rx> = End<Key, <Tx as backend::Transmitter>::Error, Tx, Tx, Rx>;

/// A resuming receiver end is an [`End`] whose `Inner` is `Rx` and whose `Error` is an `Rx::Error`.
pub(super) type ReceiverEnd<Key, Tx, Rx> = End<Key, <Rx as backend::Receiver>::Error, Rx, Tx, Rx>;

impl<Key, Err, Inner, Tx, Rx> Drop for End<Key, Err, Inner, Tx, Rx>
where
    Key: Eq + Hash,
{
    fn drop(&mut self) {
        // Remove the entire entry at this key if the other end has already dropped
        let _ = self.managed.remove_if(&self.key, |_, waiting| {
            waiting.half_dropped.swap(true, Ordering::SeqCst)
        });
        // Resize the backing map if it's now smaller than 25% capacity
        if self.managed.len().saturating_mul(4) < self.managed.capacity() {
            self.managed.shrink_to_fit();
        }
    }
}

impl<Key, Err, Inner, Tx, Rx> End<Key, Err, Inner, Tx, Rx>
where
    Key: Eq + Hash,
{
    /// Create a new [`End`] from its inner pieces.
    pub(super) fn new(
        key: Key,
        inner: Inner,
        next: maybe_bounded::Receiver<Inner>,
        recover: Arc<dyn Fn(usize, &Err) -> Recovery + Sync + Send>,
        managed: Arc<Managed<Key, Tx, Rx>>,
        timeout: Option<Duration>,
    ) -> Self {
        Self {
            key,
            inner: Some(inner),
            next,
            recover,
            managed,
            timeout,
        }
    }

    /// Given an error, interpret the contained recovery strategy and return `true` if and only if
    /// we should continue onwards. This function modifies `retries` and `deadline` to keep them
    /// updated, with the assumption that they are initialized on the first occurrence of this error
    /// at `0` and `None`, respectively.
    pub(super) async fn recover(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
        error: &Err,
    ) -> bool {
        match (self.recover)(*retries, error) {
            Recovery::Fail => return false,
            Recovery::Reconnect => self.inner = None,
        }

        // Set the deadline if it hasn't already been set and this is the first attempt
        if *retries == 0 && deadline.is_none() {
            *deadline = self.timeout.map(|delay| Instant::now() + delay);
        }

        // Note that one more retry has occurred
        *retries += 1;

        // If we got here, we didn't time out
        true
    }

    /// Attempt to acquire a mutable reference to the `Inner` thing in this end (this will be either
    /// a `Tx` or an `Rx` in practice), waiting for a new connection to be accepted if necessary.
    ///
    /// If this operation last past the specified deadline, returns `None`.
    pub(super) async fn inner(&mut self, deadline: Option<Instant>) -> Option<&mut Inner> {
        if self.inner.is_none() {
            self.inner = Some(
                timeout_at_option(deadline, self.next.recv())
                    .await
                    .ok()?
                    .expect("acceptor dropped before end that refers to it"),
            );
        }

        // At this point, `self.inner` is always `Some`
        Some(self.inner.as_mut().unwrap())
    }
}
