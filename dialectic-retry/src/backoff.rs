use std::time::Duration;

#[derive(Debug, Clone)]
pub struct Backoff {
    initial_delay: Duration,
    factor: f64,
    jitter: Duration,
    max_delay: Option<Duration>,
    max_retries: usize,
}

impl Backoff {
    pub fn with_delay(initial_delay: Duration) -> Self {
        Backoff {
            initial_delay,
            factor: 1.0,
            jitter: Duration::from_millis(0),
            max_delay: None,
            max_retries: usize::MAX,
        }
    }

    pub fn exponential(mut self, factor: f64) -> Self {
        self.factor = factor;
        self
    }

    pub fn jitter(mut self, jitter: Duration) -> Self {
        self.jitter = jitter;
        self
    }

    pub fn max_delay(mut self, max_delay: Duration) -> Self {
        self.max_delay = Some(max_delay);
        self
    }

    pub fn clear_max_delay(mut self) -> Self {
        self.max_delay = None;
        self
    }

    pub fn max_retries(mut self, max_retries: usize) -> Self {
        self.max_retries = max_retries;
        self
    }

    pub fn backoff<T, E>(
        &self,
        strategy: impl Fn(Duration) -> T + Sync + Send + 'static,
    ) -> impl Fn(usize, &E) -> T + Sync + Send + 'static
    where
        T: Default + Sync + Send + 'static,
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
                T::default()
            }
        }
    }
}
