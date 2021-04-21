use dialectic::prelude::*;
use dialectic_retry::{Connector, ReconnectStrategy, RetryStrategy};
use dialectic_tokio_serde::codec::LinesCodec;
use dialectic_tokio_serde_json::Json;
use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};

type Handshake = Session! {
    choose {
        0 => recv usize,
        1 => send usize,
    };
};

type HandshakeErr = Box<dyn std::error::Error>;

type Tx = dialectic_tokio_serde::Sender<Json, LinesCodec, OwnedWriteHalf>;

type Rx = dialectic_tokio_serde::Receiver<Json, LinesCodec, OwnedReadHalf>;

#[tokio::main]
async fn main() {
    let init = |chan: Chan<Handshake, Tx, Rx>| async move {
        let (key, _) = chan.choose::<0>().await?.recv().await?;
        Ok::<_, HandshakeErr>(key)
    };
    let retry = |key: &usize, chan: Chan<Handshake, Tx, Rx>| async move {
        let chan = chan.choose::<1>().await?;
        chan.send_ref(key).await?;
        Ok::<_, HandshakeErr>(())
    };
    let connector = Connector::new(init, retry);
}
