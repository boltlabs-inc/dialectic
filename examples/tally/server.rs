#![allow(unused)]
use std::{
    fmt::{self, Display},
    str::FromStr,
};
use thiserror::Error;

use dialectic::backend::serde::{
    format::{length_delimited_bincode, Bincode},
    Receiver, Sender, SymmetricalError,
};
use dialectic::constants::*;
use dialectic::unary::types::*;
use dialectic::*;

use serde_crate::{Deserialize, Serialize};
use tokio::net::{
    tcp::ReadHalf,
    tcp::{OwnedReadHalf, OwnedWriteHalf, WriteHalf},
    TcpListener, TcpStream,
};
use tokio_util::codec::LengthDelimitedCodec;

pub type Server =
    Loop<Recv<Operation, Loop<Offer<(Recv<i64>, Send<i64, Offer<(Break<_1>, Break)>>)>>>>;

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

// TODO: generalize this into the library to make a normalized Chan type synonym so things work out
// nicely instead of weirdly clashing because of auto-ff-ing through loops and recurs?
#[allow(type_alias_bounds)]
pub type BincodeTcpChan<P, E = ()>
where
    P: Actionable<E>,
    E: Environment,
= Chan<
    Sender<Bincode, LengthDelimitedCodec, OwnedWriteHalf>,
    Receiver<Bincode, LengthDelimitedCodec, OwnedReadHalf>,
    P::Action,
    P::Env,
>;

pub fn wrap_socket<'a, P>(mut socket: TcpStream, max_length: usize) -> BincodeTcpChan<P>
where
    P: NewSession,
    P::Dual: Actionable<()>,
    <P::Env as EachSession>::Dual: Environment,
    <<P::Dual as Actionable<()>>::Env as EachSession>::Dual: Environment,
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
    let listener = TcpListener::bind(("127.0.0.1", port)).await?;
    loop {
        let (mut socket, addr) = listener.accept().await?;
        eprintln!("Accepted: {}", addr);
        tokio::spawn(async move {
            serve(wrap_socket::<Server>(socket, 8 * 1024))
                .await
                .map(|_| eprintln!("Finished: {}", addr))
                .map_err(|err| eprintln!("Error on: {}: {}", addr, err))
        });
    }
}

async fn serve(
    mut chan_outer: BincodeTcpChan<Server>,
) -> Result<(), SymmetricalError<Bincode, LengthDelimitedCodec>> {
    'outer: loop {
        // What operation should we use?
        let (op, mut chan_inner) = chan_outer.recv().await?;

        // Reset the tally to the unit for that operation
        let mut tally = op.unit();

        // In a loop, either do...
        chan_outer = 'inner: loop {
            chan_inner = offer!(chan_inner => {
                _0 => {
                    // Receive a new item and combine it with the tally
                    let (i, chan_inner) = chan_inner.recv().await?;
                    tally = op.combine(tally, i);
                    chan_inner
                },
                _1 => {
                    // Report the final tally, then...
                    let chan_inner = chan_inner.send(&tally).await?;
                    offer!(chan_inner => {
                        // Finish the protocol
                        _0 => break 'outer chan_inner,
                        // Jump back to the outer loop and try a new tally
                        _1 => break 'inner chan_inner,
                    })
                }
            });
        };
    }
    .close();
    Ok(())
}
