use super::sealed::IsSession;
use super::*;

/// A finished session. The only thing to do with a [`Chan`](crate::Chan) when it is `Done` is to
/// drop it or, preferably, [`close`](crate::Chan::close) it.
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

impl<P, N: Unary> Then<P, N> for Done
where
    P: Lift<N>,
{
    type Combined = P::Lifted;
}

impl<N: Unary, Level: Unary> Lift<N, Level> for Done {
    type Lifted = Done;
}

#[cfg(test)]
mod tests {
    #![allow(unused)]

    /// Tests whether [`Done`] is actually [`Done`] when inside the first argument to [`Call`].
    #[test]
    fn done_in_seq() {
        use crate::prelude::*;
        use crate::types::*;

        type S = Session! {
            loop {
                call {
                    send String;
                }
                recv String;
                break;
            }
        };

        async fn serve<Tx, Rx>(chan: Chan<S, Tx, Rx>) -> Result<(), Box<dyn std::error::Error>>
        where
            Tx: std::marker::Send + Transmitter + Transmit<String>,
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
