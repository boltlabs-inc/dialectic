use std::fmt::Display;

use dialectic::backend::serde::{
    format::{length_delimited_bincode, Bincode},
    SymmetricalError,
};
use dialectic::constants::*;
use dialectic::*;

use tokio::io::{self, AsyncBufRead, AsyncBufReadExt, BufReader, Lines};
use tokio::net::TcpStream;

mod server;
use server::{get_port, wrap_socket, BincodeTcpChan, Operation, Server};
use tokio_util::codec::LengthDelimitedCodec;

// The client is the dual of the server
type Client = <Server as Session>::Dual;

/// Loop presenting a prompt until the user enters a parseable string or the input ends, returning
/// `Ok(None)` on end of input and `Err(_)` on other errors.
async fn parse_line_loop<T, E, R>(
    prompt: &str,
    lines: &mut Lines<R>,
    mut parse: impl FnMut(&str) -> Result<T, E>,
) -> io::Result<Option<T>>
where
    R: AsyncBufRead + Unpin,
{
    loop {
        eprint!("{}", prompt);
        match lines.next_line().await? {
            Some(line) => match parse(line.trim()) {
                Ok(t) => break Ok(Some(t)),
                Err(_) => continue,
            },
            None => break Ok(None),
        }
    }
}

/// Parse a string as a yes-or-no response
fn yes_or_no(s: &str) -> Result<bool, ()> {
    match s.to_uppercase().as_ref() {
        "Y" | "YES" => Ok(true),
        "N" | "NO" => Ok(false),
        _ => Err(()),
    }
}

fn number_or_tally(s: &str) -> Result<Option<i64>, ()> {
    let s = s.trim();
    if s == "" {
        Ok(None)
    } else if let Ok(n) = s.parse() {
        Ok(Some(n))
    } else {
        Err(())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Connect to the server and assemble into a transport using length-delimited bincode
    let port = get_port()?;
    let connection = TcpStream::connect(("127.0.0.1", port)).await?;
    interact(wrap_socket::<Client>(connection, 8 * 1024)).await?;
    Ok(())
}

async fn interact(
    mut chan_outer: BincodeTcpChan<Client>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Hook up stdin as a stream of lines, and stdout as a writer
    let mut stdin = BufReader::new(io::stdin()).lines();

    'outer: loop {
        // Get the desired operation from the user
        let operation = parse_line_loop("Operation (+ / *): ", &mut stdin, str::parse)
            .await?
            .unwrap_or_else(|| std::process::exit(1));

        // Send the desired operation
        let mut chan_inner = chan_outer.send(&operation).await?;

        eprintln!("Input numbers to tally (newline to tally):");
        chan_outer = 'inner: loop {
            match parse_line_loop(&format!("{} ", operation), &mut stdin, number_or_tally).await? {
                Some(Some(n)) => {
                    // Choose to repeat the inner loop, and repeat
                    chan_inner = chan_inner.choose(_0).await?.send(&n).await?;
                }
                Some(None) => {
                    // User wants to get the final tally
                    let (tally, chan_inner) = chan_inner.choose(_1).await?.recv().await?;
                    println!("= {}", tally);
                    match parse_line_loop("Another tally? (Y/n): ", &mut stdin, yes_or_no)
                        .await?
                        .unwrap_or(false)
                    {
                        true => break 'inner chan_inner.choose(_1).await?,
                        false => break 'outer chan_inner.choose(_0).await?,
                    }
                }
                None => {
                    // End of input stream, so let's cleanly finish up with the server
                    let (tally, chan_inner) = chan_inner.choose(_1).await?.recv().await?;
                    println!("= {}", tally);
                    break 'outer chan_inner.choose(_0).await?;
                }
            };
        };
    }
    .close();
    Ok(())
}
