pub mod error;
pub mod resume;
pub mod retry;

use dialectic::prelude::*;
use std::time::Duration;

#[async_trait::async_trait]
pub trait Handshake<Key, Tx, Rx>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    type Session: Session;
    type Error;

    async fn handshake(
        &self,
        chan: Chan<Self::Session, Tx, Rx>,
    ) -> Result<(ConnectKind, Key), Self::Error>;
}

pub enum ConnectKind {
    New,
    Existing,
}

pub enum ErrorStrategy {
    Retry(Duration),
    RetryAfterReconnect,
    Fail,
}
