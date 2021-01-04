use futures::Future;
use std::{
    fmt::{self, Display},
    str::FromStr,
};
use thiserror::Error;

use dialectic::backend::{
    serde::format::{length_delimited_bincode, Bincode},
    Choice, Receive, Ref, Transmit,
};
use dialectic::constants::*;
use dialectic::unary::types::*;
use dialectic::*;

use serde_crate::{Deserialize, Serialize};
use tokio::net::{
    tcp::{OwnedReadHalf, OwnedWriteHalf},
    TcpListener, TcpStream,
};
use tokio_util::codec::LengthDelimitedCodec;

// The server's API:
pub type Server = Loop<Offer<(Recv<Operation, Seq<Tally>>, Break)>>;
pub type Tally = Loop<Offer<(Recv<i64, Continue>, Send<i64, Break>)>>;

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

pub type BincodeTcpChan<P, E = ()> = backend::serde::SymmetricalChan<
    Bincode,
    LengthDelimitedCodec,
    OwnedWriteHalf,
    OwnedReadHalf,
    P,
    E,
>;

pub fn wrap_socket<'a, P>(socket: TcpStream, max_length: usize) -> BincodeTcpChan<P>
where
    P: NewSession,
    P::Dual: Actionable,
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
    listen_on::<Server, _, _>(port, 8 * 1024, &serve).await?;
    Ok(())
}

async fn listen_on<P, F, Fut>(
    port: u16,
    max_length: usize,
    interaction: &'static F,
) -> Result<(), Box<dyn std::error::Error>>
where
    F: Fn(BincodeTcpChan<P>) -> Fut + std::marker::Sync + 'static,
    Fut: Future<Output = Result<BincodeTcpChan<Done>, Box<dyn std::error::Error>>>
        + std::marker::Send,
    P: NewSession,
    P::Dual: Actionable,
    P::Env: Environment + std::marker::Send,
    P::Action: std::marker::Send,
{
    let listener = TcpListener::bind(("127.0.0.1", port)).await?;
    loop {
        let (socket, addr) = listener.accept().await?;
        tokio::spawn(async move {
            let _ = interaction(wrap_socket::<P>(socket, max_length))
                .await
                .map(|chan| chan.close())
                .map_err(|err| eprintln!("Error on {}: {}", addr, err));
        });
    }
}

async fn serve<Tx, Rx, Err>(mut chan: Chan<Tx, Rx, Server>) -> Result<Chan<Tx, Rx, Done>, Err>
where
    Rx: Receive<Operation> + Receive<i64> + Receive<Choice<_2>>,
    Tx: Transmit<i64, Ref>,
    Err: From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Rx as Receive<i64>>::Error>
        + From<<Rx as Receive<Choice<_2>>>::Error>
        + From<<Rx as Receive<Operation>>::Error>,
{
    let chan = loop {
        chan = offer!(chan => {
            // Client wants to compute another tally
            _0 => {
                let (op, chan) = chan.recv().await?;
                chan.seq(|chan| tally::<_, _, _, Err>(&op, chan)).await?.1
            },
            // Client wants to quit
            _1 => break chan,
        })
    };
    Ok(chan)
}

async fn tally<Tx, Rx, E, Err>(
    op: &Operation,
    mut chan: Chan<Tx, Rx, Tally, E>,
) -> Result<((), Chan<Tx, Rx, Done>), Err>
where
    E: Environment,
    Rx: Receive<i64> + Receive<Choice<_2>>,
    Tx: Transmit<i64, Ref>,
    Err: From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Rx as Receive<i64>>::Error>
        + From<<Rx as Receive<Choice<_2>>>::Error>,
    Break: Actionable<<Tally as Actionable<E>>::Env, Action = Done, Env = ()>,
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
            _1 => break Ok(((), chan.send(&tally).await?)),
        })
    }
}
