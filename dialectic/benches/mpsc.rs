use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use dialectic_tokio_mpsc as mpsc;
use dialectic::prelude::*;
use dialectic::Unavailable;
use std::{any::Any, sync::Arc, time::Duration};
use std::{marker, time::Instant};
use tokio::runtime::Runtime;
use tokio::sync::mpsc::channel;

type Server = Session! { loop { send bool } };
type Client = <Server as Session>::Dual;

async fn dialectic_client(mut chan: Chan<Client, Unavailable, mpsc::Receiver<'static>>) {
    while {
        let (b, c) = chan.recv().await.unwrap();
        chan = c;
        b
    } {}
}

async fn plain_client(mut rx: mpsc::Receiver<'static>) {
    while *rx.recv().await.unwrap().downcast().unwrap() {}
}

async fn dialectic_server(
    iterations: usize,
    mut chan: Chan<Server, mpsc::Sender<'static>, Unavailable>,
) {
    for _ in 0..iterations {
        chan = chan.send(true).await.unwrap();
    }
    let _ = chan.send(false).await.unwrap();
}

async fn plain_server(iterations: usize, tx: mpsc::Sender<'static>) {
    for _ in 0..iterations {
        tx.send(Box::new(true)).await.unwrap();
    }
    tx.send(Box::new(false)).await.unwrap();
}

fn bench_send_recv(c: &mut Criterion) {
    let size: usize = 1024;

    let mut dialectic_group = c.benchmark_group("dialectic");
    dialectic_group.throughput(Throughput::Elements(size as u64));

    let rt = Arc::new(Runtime::new().unwrap());

    dialectic_group.bench_with_input(BenchmarkId::new("recv", size), &size, |b, &s| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::from_secs(0);
            for _ in 0..iters {
                // Pre-populate the channel with things to receive
                let (tx, rx) = channel::<Box<dyn Any + marker::Send>>(s + 1);
                rt.block_on(plain_server(s, tx.clone()));
                let client = Client::wrap(Unavailable::default(), rx);
                let start = Instant::now();
                rt.block_on(dialectic_client(client));
                total_duration += start.elapsed();
                drop(tx);
            }
            total_duration
        });
    });

    dialectic_group.bench_with_input(BenchmarkId::new("send", size), &size, |b, &s| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::from_secs(0);
            for _ in 0..iters {
                let (tx, rx) = channel::<Box<dyn Any + marker::Send>>(s + 1);
                let server = Server::wrap(tx, Unavailable::default());
                let start = Instant::now();
                rt.block_on(dialectic_server(s, server));
                total_duration += start.elapsed();
                drop(rx);
            }
            total_duration
        });
    });

    drop(dialectic_group);

    let mut plain_group = c.benchmark_group("plain");
    plain_group.throughput(Throughput::Elements(size as u64));

    plain_group.bench_with_input(BenchmarkId::new("recv", size), &size, |b, &s| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::from_secs(0);
            for _ in 0..iters {
                // Pre-populate the channel with things to receive
                let (tx, rx) = channel::<Box<dyn Any + marker::Send>>(s + 1);
                rt.block_on(plain_server(s, tx.clone()));
                let start = Instant::now();
                rt.block_on(plain_client(rx));
                total_duration += start.elapsed();
                drop(tx);
            }
            total_duration
        });
    });

    plain_group.bench_with_input(BenchmarkId::new("send", size), &size, |b, &s| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::from_secs(0);
            for _ in 0..iters {
                let (tx, rx) = channel::<Box<dyn Any + marker::Send>>(s + 1);
                let start = Instant::now();
                rt.block_on(plain_server(s, tx));
                total_duration += start.elapsed();
                drop(rx);
            }
            total_duration
        });
    });
}

criterion_group!(benches, bench_send_recv);
criterion_main!(benches);
