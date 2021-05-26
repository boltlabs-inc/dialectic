use std::time::Duration;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A description of a backoff strategy with optional exponential delay, random jitter, maximum
/// delay, and maximum retries. This can be used to generate retry strategies for building
/// [`Connector`](crate::retry::Connector)s.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(deny_unknown_fields))]
pub struct Backoff {
    #[cfg_attr(
        all(feature = "serde", feature = "humantime_serde"),
        serde(with = "humantime_serde")
    )]
    initial_delay: Duration,
    #[cfg_attr(feature = "serde", serde(default = "defaults::exponent"))]
    exponent: f64,
    #[cfg_attr(feature = "serde", serde(default = "defaults::jitter"))]
    #[cfg_attr(
        all(feature = "serde", feature = "humantime_serde"),
        serde(with = "humantime_serde")
    )]
    jitter: Duration,
    #[cfg_attr(feature = "serde", serde(default = "defaults::max_delay"))]
    #[cfg_attr(
        all(feature = "serde", feature = "humantime_serde"),
        serde(with = "humantime_serde")
    )]
    max_delay: Option<Duration>,
    #[cfg_attr(feature = "serde", serde(default = "defaults::max_retries"))]
    max_retries: usize,
}

/// The default settings for the optional parameters, defined here as functions so they can be used
/// in the serde implementation as well as the builder.
mod defaults {
    use std::time::Duration;

    pub const fn exponent() -> f64 {
        1.0
    }

    pub const fn jitter() -> Duration {
        Duration::from_secs(0)
    }

    pub const fn max_delay() -> Option<Duration> {
        None
    }

    pub const fn max_retries() -> usize {
        usize::MAX
    }
}

impl Backoff {
    /// Create a simple [`Backoff`] which delays by `initial_delay` each time it is invoked,
    /// forever.
    pub fn with_delay(initial_delay: Duration) -> Self {
        Backoff {
            initial_delay,
            exponent: defaults::exponent(),
            jitter: defaults::jitter(),
            max_delay: defaults::max_delay(),
            max_retries: defaults::max_retries(),
        }
    }

    /// Add an exponential factor to a [`Backoff`], so that every time it is invoked, it delays for
    /// that multiple of its previous delay time.
    pub fn exponential(&mut self, factor: f64) -> &mut Self {
        self.exponent = factor;
        self
    }

    /// Add random jitter to a [`Backoff`], so that every time it is invoked, it adds or subtracts a
    /// random duration from its delay within the range specified.
    pub fn jitter(&mut self, jitter: Duration) -> &mut Self {
        self.jitter = jitter;
        self
    }

    /// Cap the maximum delay of a [`Backoff`] so that every time it is invoked, it will delay by at
    /// most `max_delay`, if otherwise it would delay more.
    pub fn max_delay(&mut self, max_delay: Option<Duration>) -> &mut Self {
        self.max_delay = max_delay;
        self
    }

    /// Set the maximum number of retries for a [`Backoff`], so that it will not retry after the
    /// specified number of attempts.
    pub fn max_retries(&mut self, max_retries: usize) -> &mut Self {
        self.max_retries = max_retries;
        self
    }

    /// Use this [`Backoff`] to generate a closure that implements the backoff strategy it
    /// describes, given a constructor for some kind of delay strategy parameterized by a
    /// `Duration`.
    ///
    /// This assumes that the [`default`](Default::default) for the given `Strategy` type represents
    /// failure.
    pub fn build<Strategy, Error>(
        &self,
        strategy: impl Fn(Duration) -> Strategy + Sync + Send + 'static,
    ) -> impl Fn(usize, &Error) -> Strategy + Sync + Send + 'static
    where
        Strategy: Default + Sync + Send + 'static,
    {
        let initial_delay = self.initial_delay;
        let factor = self.exponent;
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
