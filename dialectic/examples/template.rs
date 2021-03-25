use dialectic::prelude::*;
use std::error::Error;
use tokio::io::{BufReader, Stdin, Stdout};

mod common;
#[allow(unused)]
use common::{demo, prompt};

#[tokio::main]
async fn main() {
    demo::<Server, _, _, _, _, _, _>(&server, &client, usize::MAX).await;
}

/// The session from the client's perspective.
type Client = Session! {
    // Fill this in...
};

/// The implementation of the client.
#[Transmitter(Tx ref for /* add types needed by session here */)]
#[Receiver(Rx for /* add types needed by session here */)]
async fn client<Tx, Rx>(
    #[allow(unused)] mut input: BufReader<Stdin>,
    #[allow(unused)] mut output: Stdout,
    #[allow(unused_mut)] mut chan: Chan<Client, Tx, Rx>,
) -> Result<(), Box<dyn Error>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    // Do something with the channel...
    chan.close();
    Ok(())
}

/// The session from the server's perspective.
type Server = <Client as Session>::Dual;

/// The implementation of the server for each client connection.
#[Transmitter(Tx ref for /* add types needed by session here */)]
#[Receiver(Rx for /* add types needed by session here */ )]
async fn server<Tx, Rx>(
    #[allow(unused_mut)] mut chan: Chan<Server, Tx, Rx>,
) -> Result<(), Box<dyn Error>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    // Do something with the channel...
    chan.close();
    Ok(())
}
