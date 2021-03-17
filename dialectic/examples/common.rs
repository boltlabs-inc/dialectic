use dialectic::prelude::*;
use dialectic_tokio_serde::codec::LengthDelimitedCodec;
use dialectic_tokio_serde_bincode::Bincode;

use colored::*;
pub use serde::{Deserialize, Serialize};
use std::{error::Error, fmt::Debug, future::Future, io, process};
use structopt::StructOpt;
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, AsyncWrite, AsyncWriteExt, BufReader, Stdin, Stdout},
    net::{
        tcp::{OwnedReadHalf, OwnedWriteHalf},
        TcpListener, TcpStream,
    },
};

/// Prompt the user for a line of input, looping until they enter something that parses.
#[allow(unused)]
pub async fn prompt<T, E, R, W>(
    prompt: &str,
    input: &mut R,
    output: &mut W,
    mut parse: impl FnMut(&str) -> Result<T, E>,
) -> io::Result<T>
where
    R: AsyncBufRead + Unpin,
    W: AsyncWrite + Unpin,
{
    loop {
        output.write_all(prompt.as_bytes()).await?;
        output.flush().await?;
        let mut line = String::new();
        if 0 == input.read_line(&mut line).await? {
            break Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end of input",
            ));
        }
        match parse(line.trim()) {
            Ok(t) => break Ok(t),
            Err(_) => continue,
        }
    }
}

/// A session-typed channel over TCP using length-delimited bincode encoding for serialization.
pub type TcpChan<S> = dialectic_tokio_serde::SymmetricalChan<
    S,
    Bincode,
    LengthDelimitedCodec,
    OwnedWriteHalf,
    OwnedReadHalf,
>;

/// Wrap a raw TCP socket in a given session type, using the length delimited bincode transport
/// format/encoding.
fn wrap_socket<P>(socket: TcpStream, max_length: usize) -> TcpChan<P>
where
    P: Session,
{
    let (rx, tx) = socket.into_split();
    let (tx, rx) = dialectic_tokio_serde_bincode::length_delimited_bincode(tx, rx, 4, max_length);
    P::wrap(tx, rx)
}

/// Options for a TCP app.
#[derive(Debug, Clone, StructOpt)]
struct TcpAppOptions {
    #[structopt(subcommand)]
    party: Party,
}

/// A session-typed client/server application using TCP.
#[derive(Debug, Clone, StructOpt)]
enum Party {
    /// Run the client for this session
    Client {
        /// Connect to a server running on this port
        #[structopt(short, long, default_value = "5000")]
        port: u16,
        /// Connect to this IP address
        #[structopt(short, long, default_value = "127.0.0.1")]
        address: String,
    },
    /// Run the server for this session
    Server {
        /// Serve on this port
        #[structopt(short, long, default_value = "5000")]
        port: u16,
        /// Serve on this IP address
        #[structopt(short, long, default_value = "127.0.0.1")]
        address: String,
    },
}

/// Given client and server functions for a given session type `P`, construct a simple app that
/// behaves as either a client or a server, depending on its command line arguments, and
/// communicates over TCP.
pub async fn demo<P, Server, ServerFuture, ServerResult, Client, ClientFuture, ClientResult>(
    server: &'static Server,
    client: &'static Client,
    max_length: usize,
) where
    P: Session,
    P::Dual: Session,
    Server: Fn(TcpChan<P>) -> ServerFuture + Sync + 'static,
    Client: Fn(BufReader<Stdin>, Stdout, TcpChan<P::Dual>) -> ClientFuture + Sync + 'static,
    ServerFuture:
        Future<Output = Result<ServerResult, Box<dyn Error>>> + std::marker::Send + 'static,
    ClientFuture:
        Future<Output = Result<ClientResult, Box<dyn Error>>> + std::marker::Send + 'static,
    ServerResult: Debug,
    ClientResult: Debug,
{
    use Party::*;
    let options = TcpAppOptions::from_args();
    if let Err(e) = match options.party {
        Server { port, ref address } => {
            listen_on::<P, _, _, _>(address, port, max_length, server).await
        }
        Client { port, ref address } => {
            connect_to::<P::Dual, _, _, _>(address, port, max_length, client).await
        }
    } {
        let party_name = match options.party {
            Server { .. } => "server",
            Client { .. } => "client",
        };
        eprintln!("{}", format!("[{}] Error: {}", party_name, e).red());
        process::exit(1);
    }
}

async fn listen_on<P, F, Fut, T>(
    address: &str,
    port: u16,
    max_length: usize,
    interaction: &'static F,
) -> Result<(), Box<dyn Error>>
where
    F: Fn(TcpChan<P>) -> Fut + Sync + 'static,
    Fut: Future<Output = Result<T, Box<dyn Error>>> + std::marker::Send,
    P: Session,
    T: Debug,
{
    let listener = TcpListener::bind((address, port)).await?;
    println!(
        "{}",
        format!("[server] Listening on {:?}", listener.local_addr().unwrap()).blue()
    );
    loop {
        let (socket, addr) = listener.accept().await?;
        tokio::spawn(async move {
            let _ = interaction(wrap_socket::<P>(socket, max_length))
                .await
                .map(|result| {
                    println!(
                        "{}",
                        format!("[server] {} - Result: {:?}", addr, result).green()
                    )
                })
                .map_err(|err| {
                    eprintln!("{}", format!("[server] {} - Error: {}", addr, err).red())
                });
        });
    }
}

async fn connect_to<P, F, Fut, T>(
    address: &str,
    port: u16,
    max_length: usize,
    interaction: &'static F,
) -> Result<(), Box<dyn Error>>
where
    F: Fn(BufReader<Stdin>, Stdout, TcpChan<P>) -> Fut + Sync + 'static,
    Fut: Future<Output = Result<T, Box<dyn Error>>> + std::marker::Send,
    P: Session,
    T: Debug,
{
    let socket = TcpStream::connect((address, port)).await?;
    println!(
        "{}",
        format!(
            "[client] Connected to {:?} from {:?}",
            socket.peer_addr().unwrap(),
            socket.local_addr().unwrap()
        )
        .blue()
    );
    let stdin = BufReader::new(tokio::io::stdin());
    let stdout = tokio::io::stdout();
    let result = interaction(stdin, stdout, wrap_socket::<P>(socket, max_length)).await?;
    println!("{}", format!("[client] Result: {:?}", result).green());
    Ok(())
}
