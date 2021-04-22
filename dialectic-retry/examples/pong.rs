use anyhow::Error;
use dialectic::prelude::*;
use dialectic_retry::{Acceptor, ResumeKind, ResumeStrategy};
use dialectic_tokio_serde_json as json;
use std::{
    convert::TryFrom,
    net::IpAddr,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::net::TcpListener;

mod ping;
type Protocol = <ping::Protocol as Session>::Dual;
type Handshake = <ping::Handshake as Session>::Dual;
use ping::{Rx, Tx};

#[tokio::main]
async fn main() -> Result<(), Error> {
    let key = Arc::new(AtomicUsize::new(0));

    let handshake = move |chan: Chan<Handshake, Tx, Rx>| {
        let key = key.clone();
        async move {
            let output = offer!(in chan {
                0 => {
                    let new_key = key.fetch_add(1, Ordering::SeqCst);
                    chan.send(new_key).await?.close();
                    (ResumeKind::New, new_key)
                },
                1 => {
                    let (existing_key, chan) = chan.recv().await?;
                    chan.close();
                    (ResumeKind::Existing, existing_key)
                }
            })?;
            Ok::<_, Error>(output)
        }
    };

    let timeout = Duration::from_secs(10);

    let acceptor = Acceptor::new(handshake)
        .session::<Protocol>()
        .buffer_size(1000)
        .timeout(timeout)
        .recover_tx(|retries, error| {
            eprintln!("[TX] retries: {}, error: {}", retries, error);
            ResumeStrategy::Reconnect
        })
        .recover_rx(|retries, error| {
            eprintln!("[RX] retries: {}, error: {}", retries, error);
            ResumeStrategy::Reconnect
        });

    let listener = TcpListener::bind((IpAddr::try_from([127, 0, 0, 1]).unwrap(), 5000)).await?;

    loop {
        let (tcp_stream, _) = listener.accept().await?;
        let (rx, tx) = tcp_stream.into_split();
        let (tx, rx) = json::lines(tx, rx, 1024 * 8);
        if let Some((key, mut chan)) = acceptor.accept(tx, rx).await? {
            println!("session key: {}", key);
            tokio::spawn(async move {
                #[allow(unreachable_code)]
                Ok::<_, Error>(loop {
                    let start = Instant::now();
                    print!("pong ...");
                    chan = chan.recv().await?.1.send(()).await?;
                    println!(" ping (elapsed: {:?})", start.elapsed());
                })
            });
        }
    }
}
