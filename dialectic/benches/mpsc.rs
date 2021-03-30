use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use dialectic::prelude::*;
use dialectic::Unavailable;
use dialectic_tokio_mpsc as mpsc;
use std::{any::Any, sync::Arc, time::Duration, time::Instant};
use tokio::{runtime::Runtime, sync::mpsc as tokio_mpsc};

type Server = Session! { loop { send bool } };
type Client = <Server as Session>::Dual;

async fn dialectic_client(mut chan: Chan<Client, Unavailable, mpsc::Receiver>) {
    while {
        let (b, c) = chan.recv().await.unwrap();
        chan = c;
        b
    } {}
}

async fn plain_client(mut rx: tokio_mpsc::Receiver<Box<dyn Send + Any>>) {
    while *rx.recv().await.unwrap().downcast().unwrap() {}
}

async fn dialectic_server(iterations: usize, mut chan: Chan<Server, mpsc::Sender, Unavailable>) {
    for _ in 0..iterations {
        chan = chan.send(true).await.unwrap();
    }
    let _ = chan.send(false).await.unwrap();
}

async fn plain_server(iterations: usize, tx: tokio_mpsc::Sender<Box<dyn Send + Any>>) {
    for _ in 0..iterations {
        tx.send(Box::new(true)).await.unwrap();
    }
    tx.send(Box::new(false)).await.unwrap();
}

fn bench_send_recv(c: &mut Criterion) {
    let size: usize = 1024;

    let mut dialectic_group = c.benchmark_group("dialectic/tokio/mpsc");
    dialectic_group.throughput(Throughput::Elements(size as u64));

    let rt = Arc::new(Runtime::new().unwrap());

    dialectic_group.bench_with_input(BenchmarkId::new("recv", size), &size, |b, &s| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::from_secs(0);
            for _ in 0..iters {
                // Pre-populate the channel with things to receive
                let (tx, rx) = tokio_mpsc::channel(s + 1);
                rt.block_on(plain_server(s, tx.clone()));
                let client = Client::wrap(Unavailable::default(), mpsc::Receiver::new(rx));
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
                let (tx, rx) = mpsc::channel(s + 1);
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

    let mut plain_group = c.benchmark_group("plain/tokio/mpsc");
    plain_group.throughput(Throughput::Elements(size as u64));

    plain_group.bench_with_input(BenchmarkId::new("recv", size), &size, |b, &s| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::from_secs(0);
            for _ in 0..iters {
                // Pre-populate the channel with things to receive
                let (tx, rx) = tokio_mpsc::channel(s + 1);
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
                let (tx, rx) = tokio_mpsc::channel(s + 1);
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
