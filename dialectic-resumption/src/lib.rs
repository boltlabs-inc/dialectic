use std::{future::Future, pin::Pin};

use dialectic::prelude::*;
use dialectic::SessionIncomplete;

pub type Step<Tx, Rx, St, T, E> =
    Pin<Box<dyn Future<Output = Result<Next<Tx, Rx, St, T, E>, E>> + Send>>;

pub struct Next<Tx, Rx, St, T, E> {
    inner: NextInner<Tx, Rx, St, T, E>,
}

enum NextInner<Tx, Rx, St, T, E> {
    Done(T),
    Then(Suspended<Tx, Rx, St, T, E>),
}

struct Suspended<Tx, Rx, St, T, E> {
    state: St,
    next: Step<Tx, Rx, St, T, E>,
}

impl<Tx, Rx, St, T, E> Suspended<Tx, Rx, St, T, E>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    pub fn new<K>(resumption: K, chan: Chan<K::Session, Tx, Rx>) -> Self
    where
        K: Resumption<St, T, E>,
    {
        Suspended {
            state: resumption.clone().into(),
            next: resumption.resume(chan),
        }
    }
}

impl<Tx, Rx, St, T, E> Next<Tx, Rx, St, T, E>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
    St: Send,
    T: 'static,
    E: 'static,
{
    pub fn done(t: T) -> Self {
        Next {
            inner: NextInner::Done(t),
        }
    }

    pub fn then<K>(resumption: K, chan: Chan<K::Session, Tx, Rx>) -> Self
    where
        K: Resumption<St, T, E>,
    {
        Next {
            inner: NextInner::Then(Suspended::new(resumption, chan)),
        }
    }
}

pub trait Resumption<St, T, E>: Clone + Into<St> {
    type Session: Session;

    fn resume<Tx, Rx>(self, chan: Chan<Self::Session, Tx, Rx>) -> Step<Tx, Rx, St, T, E>
    where
        Tx: Send + 'static,
        Rx: Send + 'static;
}

pub struct Results<Tx, Rx, St, T, E> {
    pub result: Result<T, E>,
    pub channels: Result<(Tx, Rx), SessionIncomplete<Tx, Rx>>,
    pub last_state: St,
}

pub async fn run<K, Tx, Rx, St, T, E>(resumption: K, tx: Tx, rx: Rx) -> Results<Tx, Rx, St, T, E>
where
    K: Resumption<St, T, E>,
    Tx: Send + 'static,
    Rx: Send + 'static,
    St: Send,
{
    let ((last_state, result), channels) =
        <K::Session as Session>::over(tx, rx, |chan| async move {
            let mut snapshot = resumption.clone().into();
            let mut step = resumption.resume(chan);
            loop {
                match step.await {
                    Ok(Next { inner }) => match inner {
                        NextInner::Done(t) => break (snapshot, Ok(t)),
                        NextInner::Then(Suspended { next, state }) => {
                            snapshot = state;
                            step = next;
                        }
                    },
                    Err(e) => break (snapshot, Err(e)),
                }
            }
        })
        .await;
    Results {
        result,
        channels,
        last_state,
    }
}
