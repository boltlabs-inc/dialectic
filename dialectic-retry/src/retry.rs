use dialectic::{
    backend::{self, By},
    prelude::*,
    SessionIncomplete,
};
use std::{
    future::Future,
    hash::Hash,
    marker::PhantomData,
    pin::Pin,
    sync::{Arc, Weak},
    time::Duration,
};
use tokio::{pin, select, sync::mpsc, time::Instant};

use crate::{ConnectKind, Handshake};

pub enum ErrorStrategy {
    Retry(Duration),
    RetryAfterReconnect(Duration),
    Fail,
}
