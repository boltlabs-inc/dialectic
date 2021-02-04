use futures::Future;
use std::{
    fmt::{self, Debug, Display},
    str::FromStr,
};
use thiserror::Error;

use dialectic::backend::serde::format::{length_delimited_bincode, Bincode};
use dialectic::prelude::*;

use serde_crate::{Deserialize, Serialize};
use tokio::net::{
    tcp::{OwnedReadHalf, OwnedWriteHalf},
    TcpListener, TcpStream,
};
use tokio_util::codec::LengthDelimitedCodec;

// The server's API:
pub type Server = Loop<Offer<(Recv<Operation, Seq<Tally, Continue>>, Done)>>;
pub type Tally = Loop<Offer<(Recv<i64, Continue>, Send<i64, Done>)>>;

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
#[serde(crate = "serde_crate")] // we only need this because we renamed serde in Cargo.toml
pub enum Operation {
    Sum,
    Product,
}

impl Operation {
    pub fn unit(&self) -> i64 {
        match self {
            Operation::Sum => 0,
            Operation::Product => 1,
        }
    }

    pub fn combine(&self, x: i64, y: i64) -> i64 {
        match self {
            Operation::Sum => x + y,
            Operation::Product => x * y,
        }
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("couldn't parse operation")]
pub struct ParseOperationError;

impl FromStr for Operation {
    type Err = ParseOperationError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Operation::Sum),
            "*" => Ok(Operation::Product),
            _ => Err(ParseOperationError),
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Sum => write!(f, "+"),
            Operation::Product => write!(f, "*"),
        }
    }
}

pub type BincodeTcpChan<P> = dialectic::backend::serde::SymmetricalChan<
    Bincode,
    LengthDelimitedCodec,
    OwnedWriteHalf,
    OwnedReadHalf,
    P,
>;

pub fn wrap_socket<P>(socket: TcpStream, max_length: usize) -> BincodeTcpChan<P>
where
    P: Session,
    P::Dual: Session,
{
    let (rx, tx) = socket.into_split();
    let (tx, rx) = length_delimited_bincode(tx, rx, 4, max_length);
    P::wrap(tx, rx)
}

/// Try to parse a port number as the first argument to the program, returning a descriptive error
/// if this fails. In a real program, you would want to replace this with proper argument parsing.
pub fn get_port() -> Result<u16, std::io::Error> {
    if let Some(port) = std::env::args().collect::<Vec<String>>().get(1) {
        port.parse().map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "the specified port must be a number from 0 to 65535",
            )
        })
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "a port must be specified on the command line",
        ))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let port = get_port()?;
    listen_on::<Server, _>(port, 8 * 1024, serve).await?;
    Ok(())
}

async fn listen_on<P, Fut>(
    port: u16,
    max_length: usize,
    interaction: fn(BincodeTcpChan<P>) -> Fut,
) -> Result<(), Box<dyn std::error::Error>>
where
    Fut: Future<Output = Result<(), Box<dyn std::error::Error>>> + std::marker::Send + 'static,
    P: std::marker::Send + Session,
    P::Dual: Session,
{
    let listener = TcpListener::bind(("127.0.0.1", port)).await?;
    loop {
        let (socket, addr) = listener.accept().await?;
        tokio::spawn(async move {
            let wrapped = wrap_socket(socket, max_length);
            let _ = interaction(wrapped)
                .await
                .map_err(|err| eprintln!("Error on {}: {}", addr, err));
        });
    }
}

async fn serve<'a, Tx: std::marker::Send, Rx: std::marker::Send, Err>(
    mut chan: Chan<Tx, Rx, Server>,
) -> Result<(), Err>
where
    Rx: Receive<Operation> + Receive<i64> + Receive<Choice<_2>>,
    Tx: Transmit<i64, Ref>,
    Err: From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Rx as Receive<i64>>::Error>
        + From<<Rx as Receive<Choice<_2>>>::Error>
        + From<<Rx as Receive<Operation>>::Error>,
{
    loop {
        chan = offer!(chan => {
            // Client wants to compute another tally
            _0 => {
                let (op, chan) = chan.recv().await?;
                chan.seq(|chan| tally::<_, _, Err>(&op, chan)).await?.1.expect("tally finishes session")
            },
            // Client wants to quit
            _1 => break chan.close(),
        })
    }
    Ok(())
}

async fn tally<Tx, Rx, Err>(op: &Operation, mut chan: Chan<Tx, Rx, Tally>) -> Result<(), Err>
where
    Rx: Receive<i64> + Receive<Choice<_2>> + std::marker::Send + 'static,
    Tx: Transmit<i64, Ref> + std::marker::Send + 'static,
    Err: From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Rx as Receive<i64>>::Error>
        + From<<Rx as Receive<Choice<_2>>>::Error>,
{
    let mut tally = op.unit();
    loop {
        chan = offer!(chan => {
            // Client wants to add another number to the tally
            _0 => {
                let (i, chan) = chan.recv().await?;
                tally = op.combine(tally, i);
                chan
            },
            // Client wants to finish this tally
            _1 => {
                chan.send(&tally).await?.close();
                break Ok(());
            }
        })
    }
}
