use dialectic::prelude::*;
use std::{
    error::Error,
    fmt::{self, Display},
    str::FromStr,
};
use thiserror::Error;
use tokio::io::{AsyncWriteExt, BufReader, Stdin, Stdout};

mod common;
use common::{demo, prompt, Deserialize, Serialize, TcpChan};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
// pub type Client = Loop<Choose<(Send<Operation, Call<ClientTally, Continue>>, Done)>>;
pub type Client = Session! {
    loop {
        choose {
            _0 => {
                send Operation;
                call ClientTally;
            },
            _1 => break,
        }
    }
};

/// The implementation of the client.
async fn client(
    mut input: BufReader<Stdin>,
    mut output: Stdout,
    mut chan: TcpChan<Client>,
) -> Result<(), Box<dyn Error>> {
    loop {
        // Parse a desired operation from the user
        chan = if let Ok(operation) =
            prompt("Operation (+ or *): ", &mut input, &mut output, str::parse).await
        {
            let chan = chan.choose::<0>().await?.send(&operation).await?;
            output
                .write_all("Enter numbers (press ENTER to tally):\n".as_bytes())
                .await?;
            output.flush().await?;
            let (done, chan) = chan
                .call(|chan| client_tally(&operation, &mut input, &mut output, chan))
                .await?;
            let chan = chan.unwrap();
            if done {
                break chan.choose::<1>().await?;
            } else {
                chan
            }
        } else {
            // End of input, so quit
            break chan.choose::<1>().await?;
        }
    }
    .close();
    Ok(())
}

/// A sub-routine to tally a sequence of numbers.
// pub type ClientTally = Loop<Choose<(Send<i64, Continue>, Recv<i64, Done>)>>;
pub type ClientTally = Session! {
    loop {
        choose {
            _0 => send i64,
            _1 => {
                recv i64;
                break;
            }
        }
    }
};

/// The implementation of the client's tally subroutine.
async fn client_tally(
    operation: &Operation,
    input: &mut BufReader<Stdin>,
    output: &mut Stdout,
    mut chan: TcpChan<ClientTally>,
) -> Result<bool, Box<dyn Error>> {
    let (done, chan) = loop {
        // Parse a desired number from the user
        let user_input = prompt(&format!("{} ", operation), input, output, |s| {
            let s = s.trim();
            if s.is_empty() || s == "=" {
                // Empty line or "=" means finish tally
                Ok(None)
            } else if let Ok(n) = s.parse() {
                // A number means add it to the tally
                Ok(Some(n))
            } else {
                // Anything else is a parse error
                Err(())
            }
        })
        .await;
        match &user_input {
            // User wants to add another number to the tally
            Ok(Some(n)) => chan = chan.choose::<0>().await?.send(&n).await?,
            // User wants to finish this tally
            Ok(None) | Err(_) => {
                let (tally, chan) = chan.choose::<1>().await?.recv().await?;
                output
                    .write_all(format!("= {}\n", tally).as_bytes())
                    .await?;
                output.flush().await?;
                break (user_input.is_err(), chan);
            }
        }
    };
    chan.close();
    Ok(done)
}

/// The session from the server's perspective.
type Server = <Client as Session>::Dual;

/// The implementation of the server for each client connection.
async fn server(mut chan: TcpChan<Server>) -> Result<(), Box<dyn Error>> {
    loop {
        chan = offer!(chan => {
            // Client wants to compute another tally
            0 => {
                let (op, chan) = chan.recv().await?;
                chan.call(|chan| server_tally(&op, chan)).await?.1.unwrap()
            },
            // Client wants to quit
            1 => break chan.close(),
        })?;
    }
    Ok(())
}

/// The tally subroutine from teh server's perspective.
type ServerTally = <ClientTally as Session>::Dual;

/// The implementation of the server's tally subroutine.
async fn server_tally(
    op: &Operation,
    mut chan: TcpChan<ServerTally>,
) -> Result<(), Box<dyn Error>> {
    let mut tally = op.unit();
    loop {
        chan = offer!(chan => {
            // Client wants to add another number to the tally
            0 => {
                let (i, chan) = chan.recv().await?;
                tally = op.combine(tally, i);
                chan
            },
            // Client wants to finish this tally
            1 => {
                chan.send(&tally).await?.close();
                break Ok(());
            }
        })?;
    }
}

/// A tallying operation: either `+` or `*`.
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Operation {
    Sum,
    Product,
}

impl Operation {
    /// The unit of the operation.
    pub fn unit(&self) -> i64 {
        match self {
            Operation::Sum => 0,
            Operation::Product => 1,
        }
    }

    /// Apply the operation to operands, yielding a new tally.
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
