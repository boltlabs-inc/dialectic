use super::sealed::IsSession;
use super::*;

/// A finished session. The only thing to do with a [`Chan`] when it is `Done` is to drop it or,
/// preferably, [`close`](Chan::close) it.
///
/// If [`Done`] occurs within a [`Loop`], it is implicitly equivalent to [`Continue`]; that is to
/// say, the behavior of `Loop<Send<String, Done>>` is equivalent to `Loop<Send<String, Continue>>`.
/// This is overridden in the case where [`Done`] occurs inside the outermost level of the first
/// argument to [`Seq`]: that is, `Loop<Seq<Send<String, Done>, Done>>` is equivalent to
/// `Loop<Seq<Send<String, Break>, Continue>`.
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
