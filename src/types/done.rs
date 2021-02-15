use super::sealed::IsSession;
use super::*;

/// A finished session. The only thing to do with a [`Chan`] when it is `Done` is to drop it or,
/// preferably, [`close`](Chan::close) it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Done;

impl IsSession for Done {}

impl HasDual for Done {
    type DualSession = Done;
}

impl Actionable for Done {
    type NextAction = Self;
}

impl<N: Unary> Scoped<N> for Done {}

impl<P, N: Unary> Subst<P, N> for Done {
    type Substituted = Done;
}

#[cfg(test)]
mod tests {
    #![allow(unused)]

    /// Tests whether [`Done`] is actually [`Done`] when inside the first argument to [`Call`].
    #[test]
    fn done_in_seq() {
        use crate::prelude::*;

        type S = Loop<Call<Send<String, Done>, Recv<String, Done>>>;

        async fn serve<Tx, Rx>(chan: Chan<S, Tx, Rx>) -> Result<(), Box<dyn std::error::Error>>
        where
            Tx: std::marker::Send + Transmit<String, Val>,
            Rx: std::marker::Send + Receive<String>,
            Tx::Error: std::error::Error,
            Rx::Error: std::error::Error,
        {
            let chan = chan
                .call(|chan| async move {
                    chan.send("Hello!".to_string()).await?.close();
                    Ok::<_, Box<dyn std::error::Error>>(())
                })
                .await?
                .1
                .unwrap();
            let (_string, chan) = chan.recv().await?;
            chan.close();
            Ok(())
        }
    }
}
