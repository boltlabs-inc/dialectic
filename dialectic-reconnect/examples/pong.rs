// This server is meant to be paired with the `ping` client. It implements a very simple protocol
// where it receives a `()` and then sends a `()`, at regularly spaced intervals. However, the
// connection to the client can be interrupted and it will continue to function correctly,
// transparently resuming the connection when the client retries, with no intervention from the code
// written in terms of the channel.
//
// To test this program, run `cargo run --example ping` and `cargo run --example pong`
// simultaneously. You can then use the `tcpkill` program to perform a denial of service attack on
// the port through which the two programs communicate, using this command:
//
// ```
// sudo tcpkill -i lo -1 ip host 127.0.0.1 and port 5000
// ```
//
// Notice that both the client and server log errors, but do not crash. Varying the degree of
// aggressiveness in the call to `tcpkill` and the interval for `ping` may change the effectiveness
// of the denial of service attack, which may result in no connections being able to succeed.

use anyhow::Error;
use dialectic::prelude::*;
use dialectic_reconnect::resume::{Acceptor, Recovery, ResumeKind};
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
use tokio::{net::TcpListener, select, sync::mpsc};

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

    // Create an acceptor for our protocol which times out at 10 seconds and logs errors
    let mut acceptor = Acceptor::new(handshake, PongPing::default());
    acceptor
        .timeout(Some(Duration::from_secs(10)))
        .recover_tx({
            let log = log.clone();
            move |retries, error| {
                let _ = log.send(format!("[TX error] retries: {}, error: {}", retries, error));
                Recovery::Reconnect
            }
        })
        .recover_rx({
            let log = log.clone();
            move |retries, error| {
                let _ = log.send(format!("[RX error] retries: {}, error: {}", retries, error));
                Recovery::Reconnect
            }
        });

    // Initialize a TCP listener
    let listener = TcpListener::bind((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000)).await?;

    // Keep track of all the pending tasks
    let mut tasks = FuturesUnordered::new();

    // Loop forever accepting connections
    loop {
        // Concurrently do all of:
        select! {
            // Report on a completed server task
            Some((key, result)) = tasks.next() => {
                match result {
                    Ok(Ok::<_, Error>(())) => println!("[{}] complete session", key),
                    Ok(Err(error)) => eprintln!("[{}] unrecovered error: {}", key, error),
                    Err(panic) => eprintln!("[{}] panic in task: {}", key, panic),
                }
            },
            // Report a log message
            Some(message) = log_recv.recv() => eprintln!("{}", message),
            // Accept a new connection to the server
            Ok((tcp_stream, _)) = listener.accept() => {
                let (rx, tx) = tcp_stream.into_split();
                let (tx, rx) = json::lines(tx, rx, 1024 * 8);

                match acceptor.accept(tx, rx).await {
                    Err(error) => eprintln!("[accept error] {}", error),
                    Ok((key, None)) => println!("[{}] resume session", key),
                    Ok((key, Some(mut chan))) => {
                        println!("[{}] create session", key);
                        tasks.push(tokio::spawn(async move {
                            let mut n: usize = 0;
                            loop {
                                let start = Instant::now();
                                println!("[{}] {}: pong ...", key, n);
                                chan = chan.recv().await?.1.send(()).await?;
                                println!("[{}] ... ping (elapsed: {:?})", key, start.elapsed());
                                n += 1;
                            }
                        }).map(move |result| (key, result)));
                    }
                }
            },
        }
    }
}
