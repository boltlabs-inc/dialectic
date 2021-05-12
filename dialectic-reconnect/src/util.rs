use std::{future::Future, time::Duration};
use tokio::time::{error::Elapsed, Instant};

/// Sleep for a given duration, or until a given deadline expires, returning `true` if the sleep
/// completed before the deadline, or `false` if it did not.
///
/// This short-circuits and immediately returns `false` if it would exceed the deadline.
pub(crate) async fn sleep_until_or_deadline(duration: Duration, deadline: Option<Instant>) -> bool {
    let wakeup = Instant::now() + duration;
    if let Some(deadline) = deadline {
        // If we would exceed the deadline, don't wait at all
        if deadline < wakeup {
            return false;
        }
    }
    tokio::time::sleep_until(wakeup).await;
    true
}

/// If the future completes by the deadline or the deadline is `None`, return its result; otherwise,
/// return the [`Elapsed`] time.
pub(crate) async fn timeout_at_option<T>(
    deadline: Option<Instant>,
    future: impl Future<Output = T>,
) -> Result<T, Elapsed> {
    if let Some(deadline) = deadline {
        tokio::time::timeout_at(deadline, future).await
    } else {
        Ok(future.await)
    }
}
