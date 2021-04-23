use anyhow::Error;
use dialectic::prelude::*;
use dialectic_retry::{Backoff, Connector, ReconnectStrategy, RetryStrategy};
use dialectic_tokio_serde::codec::LinesCodec;
use dialectic_tokio_serde_json::{self as json, Json};
use std::{
    convert::TryFrom,
    net::IpAddr,
    time::{Duration, Instant},
};
use tokio::{
    net::{
        tcp::{OwnedReadHalf, OwnedWriteHalf},
        TcpStream,
    },
    sync::mpsc,
};

// The handshake session type
pub type Handshake = Session! {
    choose {
        // Start a new session: receive a session key from the server
        0 => recv usize,
        // Resume an existing session: submit a session key to the server
        1 => send usize,
    };
};

// The protocol we will be running: just send and receive in a loop forever
pub type PingPong = Session! {
    loop {
        send ();
        recv ();
    }
};

// The specific backend types we'll be using in this example
pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;

#[tokio::main]
async fn main() -> Result<(), Error> {
    // How to connect to an address and generate a `Tx`/`Rx` pair
    async fn connect(addr: (IpAddr, u16)) -> Result<(Tx, Rx), Error> {
        let (rx, tx) = TcpStream::connect(addr).await?.into_split();
        Ok(json::lines(tx, rx, 1024 * 8))
    }

    // How to perform the initial handshake, receiving a session key from the server
    async fn init(chan: Chan<Handshake, Tx, Rx>) -> Result<usize, Error> {
        Ok(chan.choose::<0>().await?.recv().await?.0)
    }

    // How to perform a retry handshake, submitting a session key to the server
    async fn retry(key: usize, chan: Chan<Handshake, Tx, Rx>) -> Result<(), Error> {
        let chan = chan.choose::<1>().await?;
        chan.send(key).await?.close();
        Ok(())
    }

    // To prevent logging from interfering with retrying, we move logging to a separate task
    let (log, mut log_recv) = mpsc::unbounded_channel();

    // Run a task to print all logged messages to stderr
    tokio::spawn(async move {
        while let Some(message) = log_recv.recv().await {
            eprintln!("{}", message);
        }
    });

    // The backoff strategy used during error recovery
    let backoff = Backoff::with_delay(Duration::from_millis(100))
        // .exponential(2.0)
        .jitter(Duration::from_millis(10))
        .max_delay(Duration::from_secs(1));

    // A connector for our protocol, with a 10 second timeout, which logs all errors and attempts to
    // recover using the backoff strategy for every kind of error
    let connector = Connector::new(connect, init, retry)
        .session::<PingPong>()
        .timeout(Duration::from_secs(10))
        .recover_connect({
            let log = log.clone();
            let backoff = backoff.backoff(ReconnectStrategy::ReconnectAfter);
            move |tries, error| {
                let _ = log.send(format!(
                    "[reconnect error] retries: {}, error: {}",
                    tries, error
                ));
                backoff(tries, error)
            }
        })
        .recover_handshake({
            let log = log.clone();
            let backoff = backoff.backoff(ReconnectStrategy::ReconnectAfter);
            move |tries, error| {
                let _ = log.send(format!(
                    "[handshake error] retries: {}, error: {}",
                    tries, error
                ));
                backoff(tries, error)
            }
        })
        .recover_tx({
            let log = log.clone();
            let backoff = backoff.backoff(RetryStrategy::ReconnectAfter);
            move |tries, error| {
                let _ = log.send(format!("[TX error] retries: {}, error: {}", tries, error));
                backoff(tries, error)
            }
        })
        .recover_rx({
            let log = log.clone();
            let backoff = backoff.backoff(RetryStrategy::ReconnectAfter);
            move |tries, error| {
                let _ = log.send(format!("[RX error] retries: {}, error: {}", tries, error));
                backoff(tries, error)
            }
        });

    // At program start, connect to the server
    let (key, mut chan) = connector
        .connect((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000))
        .await?;

    println!("[{}] create session", key);

    // Loop forever measuring how long it takes for the server to respond
    let mut n: usize = 0;
    loop {
        let start = Instant::now();
        println!("[{}] {}: ping ...", key, n);
        chan = chan.send(()).await?.recv().await?.1;
        println!("[{}] ... pong (elapsed: {:?})", key, start.elapsed());
        tokio::time::sleep(Duration::from_millis(100)).await;
        n += 1;
    }
}
