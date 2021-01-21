use super::sealed::IsSession;
use super::*;

/// A finished session. The only thing to do with a [`Chan`] when it is `Done` is to drop it or,
/// preferably, [`close`](CanonicalChan::close) it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Done;

impl IsSession for Done {}

impl HasDual for Done {
    type Dual = Done;
}

impl Actionable for Done {
    type Action = Self;
}

impl<N: Unary> Scoped<N> for Done {}

impl<P: 'static> Subst<P, Z, Continue> for Done {
    type Substituted = P;
}

impl<P: 'static> Subst<P, Z, Done> for Done {
    type Substituted = Done;
}

impl<P, N: Unary> Subst<P, S<N>, Continue> for Done {
    type Substituted = Done;
}

impl<P, N: Unary> Subst<P, S<N>, Done> for Done {
    type Substituted = Break<N>;
}

#[cfg(test)]
mod tests {
    #![allow(unused)]

    /// Tests whether [`Done`] is actually [`Done`] when inside the first argument to [`Seq`].
    #[test]
    fn done_in_seq() {
        use crate::prelude::*;

        type S = Loop<Seq<Send<String, Done>, Recv<String, Break>>>;

        async fn serve<Tx, Rx>(chan: Chan<Tx, Rx, S>) -> Result<(), Box<dyn std::error::Error>>
        where
            Tx: std::marker::Send + Transmit<String, Val>,
            Rx: std::marker::Send + Receive<String>,
            Tx::Error: std::error::Error,
            Rx::Error: std::error::Error,
        {
            let chan = chan
                .seq(|chan| async move {
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
