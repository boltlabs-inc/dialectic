use dialectic::backend::{Choice, Receive, Ref, Transmit, Val};
use dialectic::constants::*;
use dialectic::*;

use tokio::io::{self, AsyncBufRead, AsyncBufReadExt, AsyncWrite, AsyncWriteExt, BufReader, Lines};
use tokio::net::TcpStream;

mod server;
use server::{get_port, wrap_socket, Operation, Server};

// The client is the dual of the server
type Client = <Server as Session>::Dual;

/// Loop presenting a prompt until the user enters a parseable string or the input ends, returning
/// `Ok(None)` on end of input and `Err(_)` on other errors.
async fn parse_line_loop<T, E, R, W>(
    prompt: &str,
    lines: &mut Lines<R>,
    output: &mut W,
    mut parse: impl FnMut(&str) -> Result<T, E>,
) -> io::Result<Option<T>>
where
    R: AsyncBufRead + Unpin,
    W: AsyncWrite + Unpin,
{
    loop {
        output.write_all(prompt.as_bytes()).await?;
        match lines.next_line().await? {
            Some(line) => match parse(line.trim()) {
                Ok(t) => break Ok(Some(t)),
                Err(_) => continue,
            },
            None => break Ok(None),
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Connect to the server and assemble into a transport using length-delimited bincode
    let port = get_port()?;
    let connection = TcpStream::connect(("127.0.0.1", port)).await?;
    let mut stdin = BufReader::new(tokio::io::stdin()).lines();
    let mut stdout = tokio::io::stdout();
    interact::<_, _, _, _, Box<dyn std::error::Error>>(
        &mut stdin,
        &mut stdout,
        wrap_socket::<Client>(connection, 8 * 1024),
    )
    .await?
    .close();
    Ok(())
}

async fn interact<Tx, Rx, R, W, Err>(
    input: &mut Lines<R>,
    output: &mut W,
    mut chan: Chan<Tx, Rx, Client>,
) -> Result<Chan<Tx, Rx, Done>, Err>
where
    R: AsyncBufRead + Unpin,
    W: AsyncWrite + Unpin,
    Rx: Receive<i64>,
    Tx: Transmit<i64, Ref> + Transmit<Operation, Ref> + Transmit<Choice<_2>, Val>,
    Err: From<<Rx as Receive<i64>>::Error>
        + From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Tx as Transmit<Choice<_2>, Val>>::Error>
        + From<<Tx as Transmit<Operation, Ref>>::Error>
        + From<io::Error>,
{
    let chan = 'outer: loop {
        // Parse a desired operation from the user
        if let Some(operation) =
            parse_line_loop("Operation (+ / *): ", input, output, str::parse).await?
        {
            let mut chan_inner = chan.choose(_0).await?.send(&operation).await?;
            chan = loop {
                // Parse a desired number from the user
                match parse_line_loop(&format!("{} ", operation), input, output, |s| {
                    let s = s.trim();
                    if s == "" {
                        // Empty line means finish tally
                        Ok(None)
                    } else if let Ok(n) = s.parse() {
                        // A number means add it to the tally
                        Ok(Some(n))
                    } else {
                        // Anything else is a parse error
                        Err(())
                    }
                })
                .await?
                {
                    // User wants to add another number to the tally
                    Some(Some(n)) => chan_inner = chan_inner.choose(_0).await?.send(&n).await?,
                    // User wants to finish this tally
                    Some(None) => {
                        let (tally, chan_inner) = chan_inner.choose(_1).await?.recv().await?;
                        output.write_all(format!("= {}", tally).as_bytes()).await?;
                        break chan_inner;
                    }
                    // End of input, so finish this tally and quit
                    None => {
                        let (tally, chan_inner) = chan_inner.choose(_1).await?.recv().await?;
                        output.write_all(format!("= {}", tally).as_bytes()).await?;
                        break 'outer chan_inner.choose(_1).await?;
                    }
                }
            }
        } else {
            // End of input, so quit
            break chan.choose(_1).await?;
        }
    };
    Ok(chan)
}
