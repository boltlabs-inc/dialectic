use anyhow::Error;
use dialectic::prelude::*;
use dialectic_retry::{Acceptor, ResumeKind, ResumeStrategy};
use dialectic_tokio_serde_json as json;
use futures::{stream::FuturesUnordered, FutureExt, StreamExt};
use std::{
    convert::TryFrom,
    net::IpAddr,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::{net::TcpListener, pin, select, sync::mpsc};

mod ping;
type PongPing = <ping::PingPong as Session>::Dual;
type Handshake = <ping::Handshake as Session>::Dual;
use ping::{Rx, Tx};

#[tokio::main]
async fn main() -> Result<(), Error> {
    // We assign keys to sessions based on sequential ordering
    let key = Arc::new(AtomicUsize::new(0));

    // The handshake either generates a new key and sends it, or receives an existing key
    let handshake = move |chan: Chan<Handshake, Tx, Rx>| {
        let key = key.clone();
        async move {
            let output = offer!(in chan {
                0 => {
                    // Client wants to start a new session
                    let new_key = key.fetch_add(1, Ordering::SeqCst);
                    chan.send(new_key).await?.close();
                    (ResumeKind::New, new_key)
                },
                1 => {
                    // Client wants to resume an existing session
                    let (existing_key, chan) = chan.recv().await?;
                    chan.close();
                    (ResumeKind::Existing, existing_key)
                }
            })?;
            Ok::<_, Error>(output)
        }
    };

    // To prevent logging from interfering with retrying, we move logging to a separate task
    let (log, mut log_recv) = mpsc::unbounded_channel();

    // Task to print all logged messages to stderr
    tokio::spawn(async move {
        while let Some(message) = log_recv.recv().await {
            eprintln!("{}", message);
        }
    });

    // Create an acceptor for our protocol which times out at 10 seconds and logs errors
    let acceptor = Acceptor::new(handshake)
        .session::<PongPing>()
        .timeout(Duration::from_secs(10))
        .recover_tx({
            let log = log.clone();
            move |retries, error| {
                let _ = log.send(format!("[TX error] retries: {}, error: {}", retries, error));
                ResumeStrategy::Reconnect
            }
        })
        .recover_rx({
            let log = log.clone();
            move |retries, error| {
                let _ = log.send(format!("[RX error] retries: {}, error: {}", retries, error));
                ResumeStrategy::Reconnect
            }
        });

    // Initialize a TCP listener
    let listener = TcpListener::bind((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000)).await?;

    // Keep track of all the pending tasks
    let mut tasks = FuturesUnordered::new();

    // Loop forever accepting connections
    loop {
        let task_complete = tasks.next();
        let next_stream = async { Ok::<_, Error>(listener.accept().await?.0) };
        pin!(task_complete, next_stream);

        // Concurrently do both of:
        select! {
            // Accept a new connection to the server
            Ok(tcp_stream) = next_stream => {
                let (rx, tx) = tcp_stream.into_split();
                let (tx, rx) = json::lines(tx, rx, 1024 * 8);

                match acceptor.accept(tx, rx).await? {
                    (key, None) => println!("[{}] resume session", key),
                    (key, Some(mut chan)) => {
                        println!("[{}] create session", key);
                        tasks.push(tokio::spawn(async move {
                            let mut n: usize = 0;
                            loop {
                                let start = Instant::now();
                                print!("[{}] {}: pong ...", key, n);
                                chan = chan.recv().await?.1.send(()).await?;
                                println!(" ping (elapsed: {:?})", start.elapsed());
                                n += 1;
                            }
                        }).map(move |result| (key, result)));
                    }
                }
            },
            // Report on a completed server task
            Some((key, result)) = task_complete => {
                match result.unwrap() {
                    Ok(()) => println!("[{}] complete session", key),
                    Err::<_, Error>(error) => println!("[{}] unrecovered error: {}", key, error),
                }
            },
        }
    }
}
