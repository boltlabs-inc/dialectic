#![allow(unused)]
use dialectic::prelude::*;
use tokio::io::{self, AsyncBufRead, AsyncBufReadExt, AsyncWrite, AsyncWriteExt, BufReader, Lines};
use tokio::net::TcpStream;

mod server;
use server::{get_port, wrap_socket, Operation, Server, Tally};

// The client is the dual of the server
type Client = <Server as HasDual>::Dual;
type ClientTally = <Tally as HasDual>::Dual;

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
        output.flush().await?;
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
    Rx: Receive<i64> + std::marker::Send + 'static,
    Tx: Transmit<i64, Ref>
        + Transmit<Operation, Ref>
        + Transmit<Choice<_2>, Val>
        + std::marker::Send
        + 'static,
    Err: From<<Rx as Receive<i64>>::Error>
        + From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Tx as Transmit<Choice<_2>, Val>>::Error>
        + From<<Tx as Transmit<Operation, Ref>>::Error>
        + From<io::Error>,
{
    let chan = loop {
        // Parse a desired operation from the user
        if let Some(operation) =
            parse_line_loop("Operation (+ or *): ", input, output, str::parse).await?
        {
            let mut chan_inner = chan.choose(_0).await?.send(&operation).await?;
            output
                .write_all("Enter numbers (press ENTER to tally):\n".as_bytes())
                .await?;
            output.flush().await?;
            let (done, chan_inner) = chan_inner
                .seq(|chan| tally::<_, _, _, _, Err>(&operation, input, output, chan))
                .await?;
            let chan_inner = chan_inner.unwrap();
            if done {
                break chan_inner.choose(_1).await?;
            } else {
                chan = chan_inner;
            }
        } else {
            // End of input, so quit
            break chan.choose(_1).await?;
        }
    };
    Ok(chan)
}

async fn tally<Tx, Rx, R, W, Err>(
    operation: &Operation,
    input: &mut Lines<R>,
    output: &mut W,
    mut chan: Chan<Tx, Rx, ClientTally>,
) -> Result<bool, Err>
where
    R: AsyncBufRead + Unpin,
    W: AsyncWrite + Unpin,
    Rx: Receive<i64> + std::marker::Send + 'static,
    Tx: Transmit<i64, Ref> + Transmit<Choice<_2>, Val> + std::marker::Send + 'static,
    Err: From<<Rx as Receive<i64>>::Error>
        + From<<Tx as Transmit<i64, Ref>>::Error>
        + From<<Tx as Transmit<Choice<_2>, Val>>::Error>
        + From<io::Error>,
{
    let (done, chan) = loop {
        // Parse a desired number from the user
        match parse_line_loop(&format!("{} ", operation), input, output, |s| {
            let s = s.trim();
            if s == "" || s == "=" {
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
        .await?
        {
            // User wants to add another number to the tally
            Some(Some(n)) => chan = chan.choose(_0).await?.send(&n).await?,
            // User wants to finish this tally
            Some(None) => {
                let (tally, chan) = chan.choose(_1).await?.recv().await?;
                output
                    .write_all(format!("= {}\n", tally).as_bytes())
                    .await?;
                output.flush().await?;
                break (false, chan);
            }
            // End of input, so finish this tally and quit
            None => {
                let (tally, chan) = chan.choose(_1).await?.recv().await?;
                output
                    .write_all(format!("= {}\n", tally).as_bytes())
                    .await?;
                output.flush().await?;
                break (true, chan);
            }
        }
    };
    chan.close();
    Ok(done)
}
