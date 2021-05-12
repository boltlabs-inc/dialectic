use async_trait::async_trait;
use std::{future::Future, pin::Pin};

use dialectic::prelude::*;
use dialectic::SessionIncomplete;

// pub type Step<Tx, Rx, State, T, E> =
//     Pin<Box<dyn Future<Output = Result<Next<Tx, Rx, State, T, E>, E>> + Send>>;

pub struct Next<Tx, Rx, State, T, E> {
    inner: NextInner<Tx, Rx, State, T, E>,
}

enum NextInner<Tx, Rx, State, T, E> {
    Done(T),
    Then {
        state: State,
        next: Pin<Box<dyn Future<Output = Result<Next<Tx, Rx, State, T, E>, E>> + Send>>,
    },
}

impl<Tx, Rx, State, T, E> Next<Tx, Rx, State, T, E>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
    State: Send + 'static,
    T: 'static,
    E: 'static,
{
    pub fn done(t: T) -> Self {
        Next {
            inner: NextInner::Done(t),
        }
    }

    pub fn then<S: 'static>(step: S, chan: Chan<S::Session, Tx, Rx>) -> Next<Tx, Rx, State, T, E>
    where
        S: Step<Tx, Rx, Output = T, Error = E> + Into<State>,
    {
        Next {
            inner: NextInner::Then {
                state: step.clone().into(),
                next: step.step(chan),
            },
        }
    }
}

#[async_trait]
pub trait Step<Tx, Rx>: Clone
where
    Tx: Send + 'static,
    Rx: Send + 'static,
{
    type Session: Session;
    type Output;
    type Error;

    async fn step<State: Send + 'static>(
        self,
        chan: Chan<Self::Session, Tx, Rx>,
    ) -> Result<Next<Tx, Rx, State, Self::Output, Self::Error>, Self::Error>
    where
        Self: Into<State>;
}

// #[async_trait]
// pub trait Start<Tx, Rx>
// where
//     Tx: Send + 'static,
//     Rx: Send + 'static,
// {
//     type Output;
//     type Error;

//     async fn start<State: Send + 'static>(
//         self,
//         tx: Tx,
//         rx: Rx,
//     ) -> Result<Next<Tx, Rx, State,
//     where
//         Self: Into<State>;
// }

#[async_trait]
pub trait Persist<Key, State> {
    type Error;

    async fn load(&self, key: &Key) -> Result<State, Self::Error>;

    async fn save(&mut self, key: &Key, state: State) -> Result<(), Self::Error>;
}

#[async_trait]
pub trait Handshake<Tx: Send + 'static, Rx: Send + 'static, Key>
where
    Self::Session: Session,
{
    type Session;
    type Error;

    async fn handshake(chan: Chan<Self::Session, Tx, Rx>) -> Result<Key, Self::Error>;
}

// #[derive(Clone)]
// struct Loop;

// #[Transmitter(Tx for ())]
// #[Receiver(Rx)]
// #[async_trait]
// impl<Tx, Rx> Step<Tx, Rx> for Loop
// where
//     Tx::Error: std::error::Error,
//     Rx::Error: std::error::Error,
// {
//     type Session = Session! { loop { send () } };
//     type Output = ();
//     type Error = Box<dyn std::error::Error>;

//     async fn step<State: Send + 'static>(
//         self,
//         chan: Chan<Self::Session, Tx, Rx>,
//     ) -> Result<Next<Tx, Rx, State, Self::Output, Self::Error>, Self::Error>
//     where
//         Self: Into<State>,
//     {
//         Ok(Next::then(Loop, chan.send(()).await?))
//     }
// }

#[derive(Debug)]
pub struct Results<Tx, Rx, T, E> {
    pub result: Result<T, E>,
    pub channels: Result<(Tx, Rx), SessionIncomplete<Tx, Rx>>,
}

pub async fn handshake<H, Tx, Rx, Key>(tx: Tx, rx: Rx) -> Results<Tx, Rx, Key, H::Error>
where
    Tx: Send + 'static,
    Rx: Send + 'static,
    H: Handshake<Tx, Rx, Key>,
{
}

pub async fn resume<P, Key, S, Tx, Rx, State>(
    persistence: &mut P,
    key: &Key,
    step: S,
    tx: Tx,
    rx: Rx,
) -> Results<Tx, Rx, S::Output, S::Error>
where
    S: Step<Tx, Rx> + Into<State>,
    P: Persist<Key, State>,
    Tx: Send + 'static,
    Rx: Send + 'static,
    State: Send + 'static,
    <S as Step<Tx, Rx>>::Error: From<<P as Persist<Key, State>>::Error>,
{
    let (result, channels) = <S::Session as Session>::over(tx, rx, |chan| async move {
        let mut step = step.step(chan);
        loop {
            match step.await {
                Ok(Next { inner }) => match inner {
                    NextInner::Done(t) => break Ok(t),
                    NextInner::Then { next, state } => {
                        persistence.save(key, state).await?;
                        step = next;
                    }
                },
                Err(e) => break Err(e),
            }
        }
    })
    .await;
    Results { result, channels }
}
