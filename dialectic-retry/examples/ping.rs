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
use tokio::net::{
    tcp::{OwnedReadHalf, OwnedWriteHalf},
    TcpStream,
};

pub type Handshake = Session! {
    choose {
        0 => recv usize,
        1 => send usize,
    };
};

pub type Protocol = Session! {
    loop {
        send ();
        recv ();
    }
};

pub type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;
pub type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;

#[tokio::main]
async fn main() -> Result<(), Error> {
    async fn connect(addr: (IpAddr, u16)) -> Result<(Tx, Rx), Error> {
        let (rx, tx) = TcpStream::connect(addr).await?.into_split();
        Ok(json::lines(tx, rx, 1024 * 8))
    }

    async fn init(chan: Chan<Handshake, Tx, Rx>) -> Result<usize, Error> {
        Ok(chan.choose::<0>().await?.recv().await?.0)
    }

    async fn retry(key: usize, chan: Chan<Handshake, Tx, Rx>) -> Result<(), Error> {
        let chan = chan.choose::<1>().await?;
        chan.send(key).await?.close();
        Ok(())
    }

    let timeout = Duration::from_secs(10);

    let backoff = Backoff::with_delay(Duration::from_millis(100))
        // .exponential(2.0)
        .jitter(Duration::from_millis(10))
        .max_delay(Duration::from_secs(1));

    let connector = Connector::new(connect, init, retry)
        .session::<Protocol>()
        .timeout(timeout)
        .recover_connect({
            let b = backoff.backoff(ReconnectStrategy::ReconnectAfter);
            move |retries, error| {
                // eprintln!("[reconnect] retries: {}, error: {}", retries, error);
                b(retries, error)
            }
        })
        .recover_handshake({
            let b = backoff.backoff(ReconnectStrategy::ReconnectAfter);
            move |retries, error| {
                // eprintln!("[handshake] retries: {}, error: {}", retries, error);
                b(retries, error)
            }
        })
        .recover_tx({
            let b = backoff.backoff(RetryStrategy::ReconnectAfter);
            move |retries, error| {
                // eprintln!("[TX error] retries: {}, error: {}", retries, error);
                b(retries, error)
            }
        })
        .recover_rx({
            let b = backoff.backoff(RetryStrategy::ReconnectAfter);
            move |retries, error| {
                // eprintln!("[RX error] retries: {}, error: {}", retries, error);
                b(retries, error)
            }
        });

    let (key, mut chan) = connector
        .connect((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000))
        .await?;

    println!("session key: {}", key);

    let mut n = 0;
    loop {
        let start = Instant::now();
        print!("{}: ping ...", n);
        chan = chan.send(()).await?.recv().await?.1;
        println!(" pong (elapsed: {:?})", start.elapsed());
        tokio::time::sleep(Duration::from_millis(100)).await;
        n += 1;
    }
}
