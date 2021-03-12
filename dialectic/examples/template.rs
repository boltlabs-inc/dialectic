use dialectic::prelude::*;
use std::error::Error;
use tokio::io::{BufReader, Stdin, Stdout};

mod common;
#[allow(unused)]
use common::{demo, prompt, TcpChan};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
type Client = Session! {
    // Fill this in...
};

/// The implementation of the client.
async fn client(
    #[allow(unused)] mut input: BufReader<Stdin>,
    #[allow(unused)] mut output: Stdout,
    #[allow(unused_mut)] mut chan: TcpChan<Client>,
) -> Result<(), Box<dyn Error>> {
    // Do something with the channel...
    chan.close();
    Ok(())
}

/// The session from the server's perspective.
type Server = <Client as Session>::Dual;

/// The implementation of the server for each client connection.
async fn server(#[allow(unused_mut)] mut chan: TcpChan<Server>) -> Result<(), Box<dyn Error>> {
    // Do something with the channel...
    chan.close();
    Ok(())
}
