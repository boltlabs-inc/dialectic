use dialectic::prelude::*;
use std::error::Error;
use tokio::io::{AsyncWriteExt, BufReader, Stdin, Stdout};

mod common;
use common::{demo, prompt};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
type Client = Session! {
    send String;
    recv String;
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
    let name = prompt("What's your name? ", &mut input, &mut output, |name| {
        Ok::<_, ()>(name.to_string())
    })
    .await?;
    let chan = chan.send(&name).await?;
    let (greeting, chan) = chan.recv().await?;
    output.write_all(greeting.as_bytes()).await?;
    output.write_all(b"\n").await?;
    chan.close().await?;
    Ok(())
}

/// The session from the server's perspective.
type Server = <Client as Session>::Dual;

/// The implementation of the server for each client connection.
#[Transmitter(Tx ref for String)]
#[Receiver(Rx for String)]
async fn server<Tx, Rx>(chan: Chan<Server, Tx, Rx>) -> Result<(), Box<dyn Error>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    let (name, chan) = chan.recv().await?;
    let greeting = format!(
        "Hello, {}! Your name is {} characters long.",
        name,
        name.chars().count()
    );
    let chan = chan.send(&greeting).await?;
    chan.close().await?;
    Ok(())
}
