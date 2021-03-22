use criterion::{
    async_executor::AsyncExecutor, criterion_group, criterion_main, measurement::WallTime, Bencher,
    BenchmarkGroup, Criterion,
};
use dialectic::prelude::*;
use dialectic_null as null;
use dialectic_tokio_mpsc as mpsc;
use futures::Future;
use std::{convert::TryInto, fmt::Debug, marker, sync::Arc, time::Instant};
use tokio::runtime::Runtime;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Primitive {
    Send,
    Recv,
    Choose,
    Offer,
    Call,
    Split,
}

async fn send<Tx, Rx>(
    chan: Chan<Session! { loop { send () } }, Tx, Rx>,
) -> Chan<Session! { loop { send () } }, Tx, Rx>
where
    Tx: Transmitter<Convention = Val> + Transmit<()> + marker::Send,
    Tx::Error: Debug,
    Rx: marker::Send,
{
    chan.send(()).await.unwrap()
}

async fn recv<Tx, Rx>(
    chan: Chan<Session! { loop { recv () } }, Tx, Rx>,
) -> Chan<Session! { loop { recv () } }, Tx, Rx>
where
    Tx: marker::Send,
    Rx: Receiver + Receive<()> + marker::Send,
    Rx::Error: Debug,
{
    chan.recv().await.unwrap().1
}

async fn choose<Tx, Rx>(
    chan: Chan<Session! { loop { choose { 0 => {} } } }, Tx, Rx>,
) -> Chan<Session! { loop { choose { 0 => {} } } }, Tx, Rx>
where
    Tx: Transmitter<Convention = Val> + marker::Send,
    Tx::Error: Debug,
    Rx: marker::Send,
{
    chan.choose::<0>().await.unwrap()
}

async fn offer<Tx, Rx>(
    chan: Chan<Session! { loop { offer { 0 => {} } } }, Tx, Rx>,
) -> Chan<Session! { loop { offer { 0 => {} } } }, Tx, Rx>
where
    Tx: marker::Send,
    Rx: Receiver + marker::Send,
    Rx::Error: Debug,
{
    offer!(in chan {
        0 => chan,
    })
    .unwrap()
}

async fn call<Tx, Rx>(
    chan: Chan<Session! { loop { call {} } }, Tx, Rx>,
) -> Chan<Session! { loop { call {} } }, Tx, Rx>
where
    Tx: marker::Send,
    Rx: marker::Send,
{
    chan.call(|_| async { Ok::<_, ()>(()) })
        .await
        .unwrap()
        .1
        .unwrap()
}

async fn split<Tx, Rx>(
    chan: Chan<Session! { loop { split { -> {}, <- {} } } }, Tx, Rx>,
) -> Chan<Session! { loop { split { -> {} <- {} } } }, Tx, Rx>
where
    Tx: marker::Send,
    Rx: marker::Send,
{
    chan.split(|_, _| async { Ok::<_, ()>(()) })
        .await
        .unwrap()
        .1
        .unwrap()
}

fn bench_chan_loop<S, Tx, Rx, F, Fut, N, H, A>(
    b: &mut Bencher,
    rt: Arc<A>,
    channel: N,
    primitive: Primitive,
    f: F,
) where
    F: Fn(Chan<S, Tx, Rx>) -> Fut,
    Fut: Future<Output = Chan<S, Tx, Rx>>,
    N: Fn(Primitive, u64) -> (Tx, Rx, H),
    S: Session,
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    A: AsyncExecutor,
{
    b.iter_custom(|iters| {
        let (tx, rx, drop_after_bench) = channel(primitive, iters);
        let mut chan = S::wrap(tx, rx);
        let elapsed = rt.block_on(async {
            let start = Instant::now();
            for _ in 0..iters {
                chan = f(chan).await;
            }
            start.elapsed()
        });
        drop(drop_after_bench);
        elapsed
    });
}

fn bench_chan_loop_group<S, Tx, Rx, Fut, H, A>(
    g: &mut BenchmarkGroup<WallTime>,
    rt: Arc<A>,
    channel: fn(Primitive, u64) -> (Tx, Rx, H),
    name: &str,
    primitive: Primitive,
    f: fn(Chan<S, Tx, Rx>) -> Fut,
) where
    Fut: Future<Output = Chan<S, Tx, Rx>>,
    S: Session,
    Tx: marker::Send + 'static,
    Rx: marker::Send + 'static,
    A: AsyncExecutor,
{
    g.bench_function(name, move |b| {
        bench_chan_loop(b, rt.clone(), channel, primitive, f)
    });
}

fn bench_all_on<Tx, Rx, H, A>(
    c: &mut Criterion,
    rt_name: &str,
    rt: Arc<A>,
    backend_name: &str,
    channel: fn(Primitive, u64) -> (Tx, Rx, H),
) where
    Tx: Transmitter<Convention = Val> + Transmit<()> + marker::Send + 'static,
    <Tx as Transmitter>::Error: Debug,
    Rx: Receiver + Receive<()> + marker::Send + 'static,
    <Rx as Receiver>::Error: Debug,
    A: AsyncExecutor,
{
    use Primitive::*;
    let group_name = format!("{}/{}", rt_name, backend_name);
    let mut g = c.benchmark_group(&group_name);
    bench_chan_loop_group(&mut g, rt.clone(), channel, "send", Send, send);
    bench_chan_loop_group(&mut g, rt.clone(), channel, "recv", Recv, recv);
    bench_chan_loop_group(&mut g, rt.clone(), channel, "choose", Choose, choose);
    bench_chan_loop_group(&mut g, rt.clone(), channel, "offer", Offer, offer);
    bench_chan_loop_group(&mut g, rt.clone(), channel, "call", Call, call);
    bench_chan_loop_group(&mut g, rt, channel, "split", Split, split);
    g.finish();
}

fn bench_tokio_null(c: &mut Criterion) {
    bench_all_on(
        c,
        "tokio",
        Arc::new(Runtime::new().unwrap()),
        "null",
        |_primitive, _iters| (null::Sender::default(), null::Receiver::default(), ()),
    )
}

fn bench_tokio_mpsc(c: &mut Criterion) {
    use Primitive::*;
    bench_all_on(
        c,
        "tokio",
        Arc::new(Runtime::new().unwrap()),
        "mpsc",
        |primitive, iters| {
            let (tx0, rx0) = mpsc::unbounded_channel();
            let (tx1, rx1) = mpsc::unbounded_channel();
            // Pre-allocate responses for those operations which need responses
            match primitive {
                Send | Choose | Call | Split => {}
                Recv => {
                    for _ in 0..iters {
                        tx1.0.send(Box::new(())).unwrap();
                    }
                }
                Offer => {
                    for _ in 0..iters {
                        let zero_choice: Choice<1> = 0u8.try_into().unwrap();
                        tx1.0.send(Box::new(zero_choice)).unwrap();
                    }
                }
            };
            (tx0, rx1, (tx1, rx0))
        },
    )
}

criterion_group!(benches, bench_tokio_null, bench_tokio_mpsc);
criterion_main!(benches);
