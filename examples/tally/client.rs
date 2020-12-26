use dialectic::backend::serde::format::length_delimited_bincode;
use dialectic::constants::*;
use dialectic::*;

use tokio::io::{self, AsyncBufReadExt, BufReader};
use tokio::net::TcpStream;

mod server;
use server::{get_port, Operation, Server};

// The client is the dual of the server
type Client = <Server as Session>::Dual;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Connect to the server and assemble into a transport using length-delimited bincode
    let port = get_port()?;
    let mut connection = TcpStream::connect(("127.0.0.1", port)).await?;
    let (rx, tx) = connection.split();
    let (tx, rx) = length_delimited_bincode(tx, rx, 3, 8 * 1024);

    // Hook up stdin as a stream of lines, and stdout as a writer
    let mut stdin = BufReader::new(io::stdin()).lines();

    // Now let's use session types!
    let mut chan_outer = Client::wrap(tx, rx);
    let chan_outer = 'outer: loop {
        // Get the desired operation from the user
        let operation = loop {
            eprint!("Operation (+/*): ");
            match stdin.next_line().await? {
                Some(l) if l.trim() == "+" => break Operation::Sum,
                Some(l) if l.trim() == "*" => break Operation::Product,
                Some(_) => continue,
                None => std::process::exit(1),
            };
        };

        // Send the desired operation
        let mut chan_inner = chan_outer.send(&operation).await?;

        eprintln!("Input numbers to tally (tally with `=`):");
        chan_outer = 'inner: loop {
            let n = loop {
                let line = stdin.next_line().await?;
                if let Some(line) = line {
                    if line == "=" {
                        // User wants to get the final tally
                        let (tally, chan_inner) = chan_inner.choose(_1).await?.recv().await?;
                        println!("{}", tally);
                        loop {
                            // Ask whether to do another tally, and jump to the right place
                            eprintln!("Another tally? (Y/n)");
                            match stdin.next_line().await?.map(|s| s.to_uppercase()) {
                                Some(l) if l == "Y" || l == "YES" => {
                                    break 'inner chan_inner.choose(_1).await?
                                }
                                Some(l) if l == "N" || l == "NO" => {
                                    break 'outer chan_inner.choose(_0).await?
                                }
                                None => break 'outer chan_inner.choose(_0).await?,
                                _ => continue,
                            }
                        }
                    } else {
                        // User wants to submit another number to the tally
                        match line.parse() {
                            Ok(n) => break n,
                            Err(err) => eprintln!("Parse error: {}", err),
                        }
                    }
                } else {
                    // End of input stream, so let's cleanly finish up with the server
                    let (tally, chan_inner) = chan_inner.choose(_1).await?.recv().await?;
                    eprintln!("=");
                    println!("{}", tally);
                    break 'outer chan_inner.choose(_0).await?;
                }
            };
            // Choose to repeat the inner loop, and repeat
            chan_inner = chan_inner.choose(_0).await?.send(&n).await?;
        };
    };
    chan_outer.close();
    Ok(())
}
