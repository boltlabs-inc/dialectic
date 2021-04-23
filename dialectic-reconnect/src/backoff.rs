use std::time::Duration;

/// A description of a backoff strategy with optional exponential delay, random jitter, maximum
/// delay, and maximum retries. This can be used to generate retry strategies for building
/// [`Connector`](crate::retry::Connector)s.
#[derive(Debug, Clone)]
pub struct Backoff {
    initial_delay: Duration,
    factor: f64,
    jitter: Duration,
    max_delay: Option<Duration>,
    max_retries: usize,
}

impl Backoff {
    /// Create a simple [`Backoff`] which delays by `initial_delay` each time it is invoked,
    /// forever.
    pub fn with_delay(initial_delay: Duration) -> Self {
        Backoff {
            initial_delay,
            factor: 1.0,
            jitter: Duration::from_millis(0),
            max_delay: None,
            max_retries: usize::MAX,
        }
    }

    /// Add an exponential factor to a [`Backoff`], so that every time it is invoked, it delays for
    /// that multiple of its previous delay time.
    pub fn exponential(mut self, factor: f64) -> Self {
        self.factor = factor;
        self
    }

    /// Add random jitter to a [`Backoff`], so that every time it is invoked, it adds or subtracts a
    /// random duration from its delay within the range specified.
    pub fn jitter(mut self, jitter: Duration) -> Self {
        self.jitter = jitter;
        self
    }

    /// Cap the maximum delay of a [`Backoff`] so that every time it is invoked, it will delay by at
    /// most `max_delay`, if otherwise it would delay more.
    pub fn max_delay(mut self, max_delay: Duration) -> Self {
        self.max_delay = Some(max_delay);
        self
    }

    /// Clear the maximum delay of [`Backoff`] so that it will delay as long as its other parameters
    /// specify.
    pub fn clear_max_delay(mut self) -> Self {
        self.max_delay = None;
        self
    }

    /// Set the maximum number of retries for a [`Backoff`], so that it will not retry after the
    /// specified number of attempts.
    pub fn max_retries(mut self, max_retries: usize) -> Self {
        self.max_retries = max_retries;
        self
    }

    /// Use this [`Backoff`] to generate a closure that implements the backoff strategy it
    /// describes, given a constructor for some kind of delay strategy parameterized by a
    /// `Duration`.
    ///
    /// This assumes that the [`default`](Default::default) for the given `Strategy` type represents
    /// failure.
    pub fn backoff<Strategy, Error>(
        &self,
        strategy: impl Fn(Duration) -> Strategy + Sync + Send + 'static,
    ) -> impl Fn(usize, &Error) -> Strategy + Sync + Send + 'static
    where
        Strategy: Default + Sync + Send + 'static,
    {
        let initial_delay = self.initial_delay;
        let factor = self.factor;
        let max_retries = self.max_retries;
        let jitter = self.jitter;
        let max_delay = self.max_delay;

        move |retries, _| {
            if retries <= max_retries {
                let mut delay =
                    initial_delay.mul_f64(factor.abs().powf(retries.min(max_retries) as f64));
                delay = max_delay.unwrap_or(delay).min(delay);
                let random_jitter = jitter.mul_f64(rand::random());
                if rand::random() {
                    delay += random_jitter;
                } else {
                    delay -= random_jitter.min(delay);
                }
                strategy(delay)
            } else {
                Strategy::default()
            }
        }
    }
}
