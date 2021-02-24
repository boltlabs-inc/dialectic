/// The `offer!` macro offers a set of different protocols, allowing the other side of the channel
/// to choose with which one to proceed. This macro only works in a `Try` context, i.e. somewhere
/// the `?` operator would make sense to use.
///
/// # Notes
///
/// - You must specify exactly as many branches as there are options in the type of the
///   [`Offer`](crate::types::Offer) to which this expression corresponds, and they must be in the
///   same order as the choices are in the tuple [`Offer`](crate::types::Offer)ed.
/// - In the body of each branch, the identifier for the channel is rebound to have the session type
///   corresponding to that branch.
/// - To use `offer!` as an expression, ensure the type of every branch matches.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::backend::mpsc;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// type GiveOrTake = Choose<(Send<i64, Done>, Recv<String, Done>)>;
///
/// let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
///
/// // Spawn a thread to offer a choice
/// let t1 = tokio::spawn(async move {
///     offer!(c2 => {
///         _0 => { c2.recv().await?; },
///         _1 => { c2.send("Hello!".to_string()).await?; },
///     });
///     Ok::<_, mpsc::Error>(())
/// });
///
/// // Choose to send an integer
/// c1.choose(_0).await?.send(42).await?;
///
/// // Wait for the offering thread to finish
/// t1.await??;
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! offer {
    (
        $chan:ident => { $($t:tt)* }
    ) => (
        {
            match $crate::Chan::offer($chan).await {
                Ok(b) => $crate::offer!{@branches b, $chan, $crate::unary::Z, $($t)* },
                Err(e) => Err(e)?,
            }
        }
    );
    (
        @branches $branch:ident, $chan:ident, $n:ty, $(,)?
    ) => (
        $crate::Branches::empty_case($branch),
    );
    (
        @branches $branch:ident, $chan:ident, $n:ty, $label:expr => $code:expr $(,)?
    ) =>
    (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => {
                let _: $n = $label;
                $code
            },
            std::result::Result::Err($branch) => $crate::Branches::empty_case($branch),
        }
    );
    (
        @branches $branch:ident, $chan:ident, $n:ty, $label:expr => $code:expr, $($t:tt)+
    ) => (
        match $crate::Branches::case($branch) {
            std::result::Result::Ok($chan) => {
                let _: $n = $label;
                $code
            },
            std::result::Result::Err($branch) => {
                $crate::offer!{@branches $branch, $chan, $crate::unary::S<$n>, $($t)+ }
            },
        }
    );
}
