#![allow(unused)]
use dialectic::backend::serde::format::{length_delimited_bincode, Bincode};
use dialectic::constants::*;
use dialectic::unary::types::*;
use dialectic::*;

use serde_crate::{Deserialize, Serialize};
use tokio::net::{TcpListener, TcpStream};
use tokio_util::codec::LengthDelimitedCodec;

pub type Server =
    Loop<Recv<Operation, Loop<Offer<(Recv<i64, Recur>, Send<i64, Offer<(End, Recur<_1>)>>)>>>>;

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
#[serde(crate = "serde_crate")] // we only need this because we renamed serde in Cargo.toml
pub enum Operation {
    Sum,
    Product,
}

impl Operation {
    fn unit(&self) -> i64 {
        match self {
            Operation::Sum => 0,
            Operation::Product => 1,
        }
    }

    fn combine(&self, x: i64, y: i64) -> i64 {
        match self {
            Operation::Sum => x + y,
            Operation::Product => x * y,
        }
    }
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
            serve(socket)
                .await
                .map(|_| eprintln!("Finished: {}", addr))
                .map_err(|err| eprintln!("Error on: {}: {}", addr, err))
        });
    }
}

async fn serve(
    mut socket: TcpStream,
) -> Result<(), backend::serde::Error<Bincode, Bincode, LengthDelimitedCodec, LengthDelimitedCodec>>
{
    // Assemble a length-delimited bincode transport...
    let (rx, tx) = socket.split();
    let (tx, rx) = length_delimited_bincode(tx, rx, 3, 8 * 1024);

    // Now let's use session types!
    let mut chan_outer = Server::wrap(tx, rx);
    let chan_outer = 'outer: loop {
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
    };

    // Prove to the type system that we completed the protocol
    chan_outer.close();
    Ok(())
}
