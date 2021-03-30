#![allow(clippy::type_complexity)]
use dialectic::prelude::*;
use std::{error::Error, future::Future, pin::Pin};
use tokio::io::{AsyncWriteExt, BufReader, Stdin, Stdout};

mod common;
use common::{demo, prompt};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
type Client = Session! {
    loop {
        choose {
            0 => break,
            1 => {
                send String;
                call { continue };
                recv String;
            }
        }
    }
};

/// The implementation of the client.
#[Transmitter(Tx ref for String)]
#[Receiver(Rx for String)]
async fn client<Tx, Rx>(
    mut input: BufReader<Stdin>,
    mut output: Stdout,
    chan: Chan<Client, Tx, Rx>,
) -> Result<(), Box<dyn Error>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    // Invoke the recursive client function...
    client_rec(0, &mut input, &mut output, chan).await
}

/// The prompt function for the client.
async fn client_prompt(
    input: &mut BufReader<Stdin>,
    output: &mut Stdout,
    size: usize,
) -> Result<String, std::io::Error> {
    prompt(
        &format!(
            "[size: {}] Type a string to push, or [ENTER] to pop/quit: ",
            size
        ),
        input,
        output,
        |s| Ok::<_, ()>(s.to_string()),
    )
    .await
}

/// Recursive client implementing the protocol. This is a separate function because it needs to take
/// the additional parameter to track the stack height, and needs to take the input/output by
/// reference instead of by value. This function can't be written in `async fn` style because it is
/// recursive, and current restrictions in Rust mean that recursive functions returning futures must
/// explicitly return a boxed `dyn Future` object.
#[Transmitter(Tx ref for String)]
#[Receiver(Rx for String)]
fn client_rec<'a, Tx, Rx>(
    size: usize,
    input: &'a mut BufReader<Stdin>,
    output: &'a mut Stdout,
    mut chan: Chan<Client, Tx, Rx>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + Send + 'a>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    Box::pin(async move {
        loop {
            chan = {
                // Get either a string to push or an instruction to pop ([ENTER]) from user
                let string = client_prompt(input, output, size).await?;
                if string.is_empty() {
                    // Break this nested loop (about to go to pop/quit)
                    break chan.choose::<0>().await?.close().await?;
                } else {
                    // Push the string to the stack
                    let chan = chan.choose::<1>().await?.send(&string).await?;
                    // Recursively do `Client`
                    let chan = chan
                        .call(|chan| client_rec(size + 1, input, output, chan))
                        .await?
                        .1
                        .unwrap();
                    // Receive a popped string from the stack
                    let (string, chan) = chan.recv().await?;
                    // Print it
                    output.write_all(string.as_bytes()).await?;
                    output.write_all(b"\n").await?;
                    // Repeat the loop
                    chan
                }
            }
        }
        Ok(())
    })
}

/// The session from the server's perspective.
type Server = <Client as Session>::Dual;

/// The implementation of the server for each client connection. This function can't be written in
/// `async fn` style because it is recursive, and current restrictions in Rust mean that recursive
/// functions returning futures must explicitly return a boxed `dyn Future` object.
#[Transmitter(Tx ref for String)]
#[Receiver(Rx for String)]
fn server<Tx, Rx>(
    mut chan: Chan<Server, Tx, Rx>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + Send>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    Box::pin(async move {
        loop {
            chan = offer!(in chan {
                // Client doesn't want to push a value
                0 => break chan.close().await?,
                // Client wants to push a value
                1 => {
                    let (string, chan) = chan.recv().await?;        // Receive pushed value
                    let chan = chan.call(server).await?.1.unwrap(); // Recursively do `Server`
                    chan.send(&string.to_uppercase()).await?        // Send back that pushed value
                },
            })?;
        }
        Ok(())
    })
}
