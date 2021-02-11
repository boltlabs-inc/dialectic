use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};
use dialectic::backend::mpsc;
use dialectic::prelude::*;
use std::any::Any;
use std::marker;
use tokio::runtime::Runtime;
use tokio::sync::mpsc::unbounded_channel;

type Server = Loop<Send<bool, Continue>>;
type Client = <Server as Session>::Dual;

async fn dialectic_client(mut chan: Chan<Client, Unavailable, mpsc::UnboundedReceiver<'static>>) {
    while {
        let (b, c) = chan.recv().await.unwrap();
        chan = c;
        b
    } {}
}

async fn plain_client(mut rx: mpsc::UnboundedReceiver<'static>) {
    while *rx.recv().await.unwrap().downcast().unwrap() {}
}

async fn dialectic_server(
    iterations: usize,
    mut chan: Chan<Server, mpsc::UnboundedSender<'static>, Unavailable>,
) {
    for _ in 0..iterations {
        chan = chan.send(true).await.unwrap();
    }
    let _ = chan.send(false).await.unwrap();
}

fn plain_server(iterations: usize, tx: mpsc::UnboundedSender<'static>) {
    for _ in 0..iterations {
        tx.send(Box::new(true)).unwrap();
    }
    tx.send(Box::new(false)).unwrap();
}

fn bench_send_recv(c: &mut Criterion) {
    let size: usize = 1024;

    let mut dialectic_group = c.benchmark_group("dialectic");
    dialectic_group.throughput(Throughput::Elements(size as u64));

    dialectic_group.bench_with_input(BenchmarkId::new("recv", size), &size, |b, &s| {
        b.to_async(Runtime::new().unwrap()).iter_batched(
            || {
                // Pre-populate the channel with things to receive
                let (tx, rx) = unbounded_channel::<Box<dyn Any + marker::Send>>();
                plain_server(s, tx.clone());
                (tx, rx)
            },
            move |(tx, rx)| async move {
                let client = Client::wrap(Unavailable, rx);
                dialectic_client(client).await;
                drop(tx);
            },
            BatchSize::SmallInput,
        );
    });

    dialectic_group.bench_with_input(BenchmarkId::new("send", size), &size, |b, &s| {
        b.to_async(Runtime::new().unwrap()).iter(|| async {
            let (tx, rx) = unbounded_channel::<Box<dyn Any + marker::Send>>();
            let server = Server::wrap(tx, Unavailable);
            dialectic_server(s, server).await;
            drop(rx);
        });
    });

    drop(dialectic_group);

    let mut plain_group = c.benchmark_group("plain");
    plain_group.throughput(Throughput::Elements(size as u64));

    plain_group.bench_with_input(BenchmarkId::new("recv", size), &size, |b, &s| {
        b.to_async(Runtime::new().unwrap()).iter_batched(
            || {
                // Pre-populate the channel with things to receive
                let (tx, rx) = unbounded_channel::<Box<dyn Any + marker::Send>>();
                plain_server(s, tx.clone());
                (tx, rx)
            },
            move |(tx, rx)| async move {
                plain_client(rx).await;
                drop(tx);
            },
            BatchSize::SmallInput,
        );
    });

    plain_group.bench_with_input(BenchmarkId::new("send", size), &size, |b, &s| {
        b.to_async(Runtime::new().unwrap()).iter(|| async {
            let (tx, rx) = unbounded_channel::<Box<dyn Any + marker::Send>>();
            plain_server(s, tx);
            drop(rx);
        });
    });
}

criterion_group!(benches, bench_send_recv);
criterion_main!(benches);
