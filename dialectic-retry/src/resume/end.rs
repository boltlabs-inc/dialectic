use dialectic::backend;
use std::{
    hash::Hash,
    sync::{atomic::Ordering, Arc},
    time::Duration,
};
use tokio::{sync::mpsc, time::Instant};

use super::{Managed, ResumeStrategy};
use crate::util::{sleep_until_or_deadline, timeout_at_option};

pub struct End<Key, Err, Inner, Tx, Rx>
where
    Key: Eq + Hash,
{
    key: Key,
    inner: Option<Inner>,
    next: mpsc::Receiver<Inner>,
    recover: Arc<dyn Fn(usize, &Err) -> ResumeStrategy + Sync + Send>,
    managed: Arc<Managed<Key, Tx, Rx>>,
    timeout: Option<Duration>,
}

pub type SenderEnd<Key, Tx, Rx> = End<Key, <Tx as backend::Transmitter>::Error, Tx, Tx, Rx>;

pub type ReceiverEnd<Key, Tx, Rx> = End<Key, <Rx as backend::Receiver>::Error, Rx, Tx, Rx>;

impl<Key, Err, Inner, Tx, Rx> Drop for End<Key, Err, Inner, Tx, Rx>
where
    Key: Eq + Hash,
{
    fn drop(&mut self) {
        // Remove the entire entry at this key if the other end has already dropped
        self.managed.remove_if(&self.key, |_, waiting| {
            waiting.half_dropped.swap(true, Ordering::SeqCst)
        });
    }
}

impl<Key, Err, Inner, Tx, Rx> End<Key, Err, Inner, Tx, Rx>
where
    Key: Eq + Hash,
{
    pub(super) fn new(
        key: Key,
        inner: Inner,
        next: mpsc::Receiver<Inner>,
        recover: Arc<dyn Fn(usize, &Err) -> ResumeStrategy + Sync + Send>,
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

    pub async fn recover(
        &mut self,
        retries: &mut usize,
        deadline: &mut Option<Instant>,
        error: &Err,
    ) -> bool {
        match (self.recover)(*retries, error) {
            ResumeStrategy::Fail => return false,
            ResumeStrategy::Reconnect => self.inner = None,
            ResumeStrategy::RetryAfter(after) => {
                if !sleep_until_or_deadline(after, *deadline).await {
                    return false;
                }
            }
        }

        if *retries == 0 && deadline.is_none() {
            *deadline = self.timeout.map(|delay| Instant::now() + delay);
        }

        *retries += 1;

        true
    }

    pub async fn inner(&mut self, deadline: Option<Instant>) -> Option<&mut Inner> {
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
