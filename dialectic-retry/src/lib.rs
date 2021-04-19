pub mod error;
pub mod resume;
pub mod retry;

use dialectic::prelude::*;
use std::{future::Future, pin::Pin};

pub enum ConnectKind {
    New,
    Existing,
}
