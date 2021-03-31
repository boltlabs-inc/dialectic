use std::{future::Future, pin::Pin};

use dialectic::prelude::*;
use dialectic::SessionIncomplete;

pub type Step<Tx, Rx, State, T, E> =
    Pin<Box<dyn Future<Output = Result<Next<Tx, Rx, State, T, E>, E>> + Send>>;

pub struct Next<Tx, Rx, State, T, E> {
    inner: NextInner<Tx, Rx, State, T, E>,
}

enum NextInner<Tx, Rx, State, T, E> {
    Done(T),
    Then {
        state: State,
        next: Step<Tx, Rx, State, T, E>,
    },
}

impl<Tx, Rx, State, T, E> Next<Tx, Rx, State, T, E>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
    State: Send,
    T: 'static,
    E: 'static,
{
    pub fn done(t: T) -> Self {
        Next {
            inner: NextInner::Done(t),
        }
    }

    pub fn then<K>(
        resumption: K,
        chan: Chan<K::Session, Tx, Rx>,
    ) -> Next<Tx, Rx, State, K::Output, K::Error>
    where
        K: Resumption<State>,
    {
        Next {
            inner: NextInner::Then {
                state: resumption.clone().into(),
                next: resumption.resume(chan),
            },
        }
    }
}

pub trait Resumption<State>: Clone + Into<State> {
    type Session: Session;
    type Output;
    type Error;

    fn resume<Tx, Rx>(
        self,
        chan: Chan<Self::Session, Tx, Rx>,
    ) -> Step<Tx, Rx, State, Self::Output, Self::Error>
    where
        Tx: Send + 'static,
        Rx: Send + 'static;
}

pub struct Results<Tx, Rx, State, T, E> {
    pub result: Result<T, E>,
    pub channels: Result<(Tx, Rx), SessionIncomplete<Tx, Rx>>,
    pub last_state: State,
}

pub async fn run<K, Tx, Rx, State>(
    resumption: K,
    tx: Tx,
    rx: Rx,
) -> Results<Tx, Rx, State, K::Output, K::Error>
where
    K: Resumption<State>,
    Tx: Send + 'static,
    Rx: Send + 'static,
    State: Send,
{
    let ((last_state, result), channels) =
        <K::Session as Session>::over(tx, rx, |chan| async move {
            let mut snapshot = resumption.clone().into();
            let mut step = resumption.resume(chan);
            loop {
                match step.await {
                    Ok(Next { inner }) => match inner {
                        NextInner::Done(t) => break (snapshot, Ok(t)),
                        NextInner::Then { next, state } => {
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
