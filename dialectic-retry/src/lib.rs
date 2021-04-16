pub mod error;
pub mod resume;
pub mod retry;

use dialectic::prelude::*;
use std::{future::Future, pin::Pin};

type Handshake<H, Key, E, Tx, Rx> =
    dyn Fn(Chan<H, Tx, Rx>) -> Pin<Box<dyn Future<Output = Result<(ConnectKind, Key), E>>>>;

pub enum ConnectKind {
    New,
    Existing,
}
