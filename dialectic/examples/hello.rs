use dialectic::prelude::*;
use std::error::Error;
use tokio::io::{AsyncWriteExt, BufReader, Stdin, Stdout};

mod common;
use common::{demo, prompt, TcpChan};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
type Client = session! {
    send String;
    recv String;
};

/// The implementation of the client.
async fn client(
    mut input: BufReader<Stdin>,
    mut output: Stdout,
    chan: TcpChan<Client>,
) -> Result<(), Box<dyn Error>> {
    let name = prompt("What's your name? ", &mut input, &mut output, |name| {
        Ok::<_, ()>(name.to_string())
    })
    .await?;
    let chan = chan.send(&name).await?;
    let (greeting, chan) = chan.recv().await?;
    output.write_all(greeting.as_bytes()).await?;
    output.write_all(b"\n").await?;
    chan.close();
    Ok(())
}

/// The session from the server's perspective.
type Server = <Client as Session>::Dual;

/// The implementation of the server for each client connection.
async fn server(chan: TcpChan<Server>) -> Result<(), Box<dyn Error>> {
    let (name, chan) = chan.recv().await?;
    let greeting = format!(
        "Hello, {}! Your name is {} characters long.",
        name,
        name.chars().count()
    );
    let chan = chan.send(&greeting).await?;
    chan.close();
    Ok(())
}
