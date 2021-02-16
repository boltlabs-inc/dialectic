use dialectic::prelude::*;
use std::{error::Error, future::Future, pin::Pin};
use tokio::io::{AsyncWriteExt, BufReader, Stdin, Stdout};

mod common;
use common::{demo, prompt, TcpChan};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
type Client = Loop<Choose<(Done, Send<String, Call<Continue, Recv<String, Continue>>>)>>;

/// The implementation of the client.
async fn client(
    mut input: BufReader<Stdin>,
    mut output: Stdout,
    chan: TcpChan<Client>,
) -> Result<(), Box<dyn Error>> {
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
fn client_rec<'a>(
    size: usize,
    input: &'a mut BufReader<Stdin>,
    output: &'a mut Stdout,
    mut chan: TcpChan<Client>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + std::marker::Send + 'a>> {
    Box::pin(async move {
        loop {
            chan = {
                // Get either a string to push or an instruction to pop ([ENTER]) from user
                let string = client_prompt(input, output, size).await?;
                if string == "" {
                    // Break this nested loop (about to go to pop/quit)
                    break chan.choose(_0).await?.close();
                } else {
                    // Push the string to the stack
                    let chan = chan.choose(_1).await?.send(&string).await?;
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
fn server(
    mut chan: TcpChan<Server>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + std::marker::Send>> {
    Box::pin(async move {
        loop {
            chan = offer!(chan => {
                // Client doesn't want to push a value
                _0 => break chan.close(),
                // Client wants to push a value
                _1 => {
                    let (string, chan) = chan.recv().await?;       // Receive pushed value
                    let chan = chan.call(server).await?.1.unwrap(); // Recursively do `Server`
                    chan.send(&string.to_uppercase()).await?       // Send back that pushed value
                },
            })
        }
        Ok(())
    })
}
