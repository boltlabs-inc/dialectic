/*! The introductory tutorial for Dialectic (nothing is exported from this module).

# Getting started with Dialectic

The first step to communicating is figuring out what you mean to say.

This crate provides a concise domain specific language for expressing session types: the
[`Session!`](macro@crate::Session) macro. The session type attached to a wrapped communications
channel indicates precisely which actions are available for each end of the channel. Internally,
Dialectic uses a set of [ordinary Rust types](crate::types) to represent session types, but it's
much easier to understand a protocol (especially a larger one) described in terms of the
[`Session!`](macro@crate::Session) macro.

Let's write our first session type:

```
# use dialectic::prelude::*;
type JustSendOneString = Session! {
    send String;
};
```

This type specifies a very simple protocol: "send a string, then finish."

Every session type has a [`Dual`](crate::Session::Dual), which describes what the other end of
the channel must do to follow the channel's protocol; if one end `send`s, the other end must
`recv`:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
assert_type_eq_all!(
    <Session! { send String } as Session>::Dual,
    Session! { recv String },
);
```

(Here and elsewhere in this tutorial, we use the
[`assert_type_eq_all!`](static_assertions::assert_type_eq_all) macro from the
[`static_assertions`] crate to assert that Rust sees these types as equal.)

Given a valid session type, we can wrap an underlying communications channel with it. Here,
let's make two ends of a channel for playing out our `JustSendOneString` protocol.

In this case, we're using the [`dialectic_tokio_mpsc`] backend crate for Dialectic, which
is built on [`tokio::sync::mpsc`][mpsc]. However, the mechanism for wrapping underlying channels is
extensible, meaning you can choose your own transport if you want.

[mpsc]: https://docs.rs/tokio/latest/tokio/sync/mpsc/index.html/

```
use dialectic_tokio_mpsc as mpsc;
```

The static method [`channel`](crate::Session::channel) is automatically defined for all valid
session types (note: its corresponding trait [`Session`] needs to be in scope for it to be
callable). It takes as input a closure that creates some underlying unidirectional transport
channel, and creates a matched pair of bidirectional session-typed channels wrapping the
underlying channel type.

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
```

The types of `c1` and `c2` are inferred from the session type specified: `c1`'s type corresponds
to the given session type, and `c2`'s type corresponds to its dual:

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
# let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
let _: Chan<Session! { send String }, mpsc::Sender, mpsc::Receiver> = c1;
let _: Chan<Session! { recv String }, mpsc::Sender, mpsc::Receiver> = c2;
```

Now that we have the two ends of a bidirectional session-typed channel, we can use them to
concurrently enact the protocol specified by their type. In this case, we're going to run them
in two parallel [Tokio] tasks. However, Dialectic is generic over the underlying async
runtime, provided the underlying transport channel is of a compatible type.

[Tokio]: https://tokio.rs

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
# let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
tokio::spawn(async move {
    c1.send("Hello, world!".to_string()).await?;
    Ok::<_, mpsc::Error>(())
});

let (greeting, c2) = c2.recv().await?;
assert_eq!(greeting, "Hello, world!");
# Ok(())
# }
```

🎉 **Tada!** We've just written an asynchronous session-typed program with Dialectic!

# Moving forward

Almost every operation on a [`Chan`]:

- is **asynchronous** due to the inherent asynchrony of the protocol,
- is **fallible** to account for issues in the underlying transport channel,
- and **takes ownership** of the `Chan` upon which it is invoked to enforce type correctness.

As a result, each invocation of an operation on a channel is usually followed by an `.await?`,
and the result of each operation should usually be rebound via `let` to the same name as the
original channel.

In this example, two interlocking threads collaborate to compute the parity of the length of the
string "Hello, world!". Notice how each time a channel operation is invoked, its name is rebound
to a new channel.

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
type ParityOfLength = Session! {
    send String;
    recv usize;
    send bool;
};

let (c1, c2) = ParityOfLength::channel(|| mpsc::channel(1));

// The string whose length's parity we'll compute
let string = "Hello, world!".to_string();

// Send "Hello, world!", receive its length, then send its parity
let t1 = tokio::spawn(async move {
    // `send` returns the channel
    let c1 = c1.send(string).await?;

    // `recv` returns a pair of (received value, channel)
    let (len, c1) = c1.recv().await?;

    // `send` returns the channel
    let c1 = c1.send(len % 2 == 0).await?;

    Ok::<_, mpsc::Error>(())
});

// Receive a string, send its length, and receive its parity
let t2 = tokio::spawn(async move {
    // `recv` returns a pair of (received value, channel)
    let (string, c2) = c2.recv().await?;

    // `send` returns the channel
    let c2 = c2.send(string.chars().count()).await?;

    // `recv` returns a pair of (received value, channel)
    let (parity, c2) = c2.recv().await?;

    Ok::<_, mpsc::Error>(parity)
});

// Wait for the tasks to finish
t1.await??;
let parity = t2.await??;

// Check that the parity was correct
assert_eq!(parity, false);
# Ok(())
# }
```

# Getting stopped

The most important benefit to session typing is not what it lets you do, but rather, what it
*prevents you from doing*. Below are some things we *can't do* with our `JustSendOneString`
channel from above:

```
# use dialectic::prelude::*;
type JustSendOneString = Session! { send String };
```

Trying to do any of the below things results in a compile-time type error (edited below for
brevity). Note that the errors below are in terms of the underlying [session types](crate::types)
to which the [`Session!`](macro@crate::Session) macro compiles its input. The types in these error
messages can be quite long, as they contain the full session type describing the channel. Debugging
such errors, you usually don't have to understand the whole verbose session type, though. You just
need to see which part doesn't match. Like so:

If we try to send on the end of the channel that's meant to receive...

```compile_fail
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
c2.send("Hello, world!".to_string()).await?;
# Ok(())
# }
```

...we get an error telling us exactly that:

```text
error[E0271]: type mismatch resolving `<Recv<String, Done> as Session>::Action == Send<_, _>`
  |
  | c2.send("Hello, world!".to_string()).await?;
  |    ^^^^ expected struct `Recv`, found struct `Send`
  |
  = note: expected struct `Recv<String, Done>`
             found struct `Send<_, _>`
```

If we try to receive the wrong type of thing...

```compile_fail
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
c1.send("Hello, world!".to_string()).await?;
let (n, c2): (i64, _) = c2.recv().await?;
# Ok(())
# }
```

...we get an error informing us so:

```text
error[E0308]: try expression alternatives have incompatible types
   |
   | let (n, c2): (i64, _) = c2.recv().await?;
   |                         ^^^^^^^^^^^^^^^^ expected `i64`, found struct `String`
```

But the unique power session types bring to the table is enforcing the validity of *sequences*
of messages. If we try to send twice in a row...

```compile_fail
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
let c1 = c1.send("Hello, world!".to_string()).await?;
c1.send("Hello again!".to_string()).await?;
# Ok(())
# }
```

...we get an error we saying the returned channel is now of type `Chan<Done, _, _>`, and we
can't send when it's the end:

```text
error[E0271]: type mismatch resolving `<Done as Session>::Action == Send<_, _>`
   |
10 | c1.send("Hello again!".to_string()).await?;
   |    ^^^^ expected struct `Done`, found struct `Send`
   |
   = note: expected struct `Done`
              found struct `Send<_, _>`
```

# Branching out

Most interesting protocols don't just consist of linear sequences of `send`s and `recv`s.
Sometimes, one party offers the other a choice of different ways to proceed, and the other
chooses which path to take.

In Dialectic, this possibility is represented by the [`Session!`](macro@crate::Session) macro's
`offer` and `choose` keywords. Each is parameterized by an ordered list of session types, each
corresponding to one possible next session.

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
type P = Session! {
    offer {
        0 => send String,
        1 => recv i64,
    }
};

type Q = Session! {
    choose {
        0 => recv String,
        1 => send i64,
    }
};

assert_type_eq_all!( <P as Session>::Dual, Q);
```

Just as the [`send`](Chan::send) and [`recv`](Chan::recv) methods enact the `send` and
`recv` session types, the [`choose`](Chan::choose) method and [`offer!`](crate::offer) macro
enact the `choose` and `offer` session types.

Suppose we want to offer a choice between two protocols: either sending a single integer
(`send i64`) or receiving a string (`recv String`). Correspondingly, the other end
of the channel must indicate a choice of which protocol to follow, and we need to handle the
result of either selection by enacting the protocol chosen.

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type JustSendOneString = Session! { send String };
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {

type P = Session! {
    offer {
        0 => send i64,
        1 => recv String,
    }
};

let (c1, c2) = P::channel(|| mpsc::channel(1));

// Offer a choice
let t1 = tokio::spawn(async move {
    let c1 = offer!(in c1 {
        0 => c1.send(42).await?,  // handle `c2.choose::<0>()`
        1 => c1.recv().await?.1,  // handle `c2.choose::<1>()`
    })?;
#   c1.close();
    Ok::<_, mpsc::Error>(())
});

// Make a choice
let t2 = tokio::spawn(async move {
    let c2 = c2.choose::<1>().await?;            // select to `Send<String, Done>`
    c2.send("Hi there!".to_string()).await?;  // enact the selected choice
    Ok::<_, mpsc::Error>(())
});

// Wait for the tasks to finish
t1.await??;
t2.await??;
# Ok(())
# }
```

In the above, we can see how the [`offer!`](crate::offer) macro takes the name of a channel
(this must be an identifier, not an expression) and a list of branches labeled by index which
may use that channel. In each expression, the channel's type corresponds to the session type for
that choice. The type of each expression in the list must be the same, which means that if we
want to bind a channel name to the result of the `offer!`, each expression must step the channel
forward to an identical session type (in the case above, that's `Done`).

Dually, to select an offered option, you can call the [`choose`](Chan::choose) method on a
channel, passing it as its type parameter a numerical constant corresponding to the index of the choice. This is done using "turbofish" syntax, for example `chan.choose::<N>()` to choose the `N`th choice.

# Looping back

While some protocols can be described as a finite sequence of operations, many contain the
possibility of loops. Consider things like retrying after an invalid response, sending an
unbounded stream of messages, or providing a persistent connection for multiple queries. All
these can be modeled with session types by introducing *iteration*.

Suppose we want to send a stream of as many integers as desired, then receive back their sum. We
could describe this protocol using the `loop` and `break` keywords in the
[`Session!`](macro@crate::Session) macro:

```
# use dialectic::prelude::*;
type QuerySum = Session! {
    loop {
        choose {
            0 => send i64,
            1 => {
                recv i64;
                break;
            }
        }
    }
};
```

**Notice:** in the [`Session!`](macro@crate::Session) macro, `loop`s repeat themselves by default,
unless explicitly broken by a `break` statement (just like in Rust!).

By taking the dual of each part of the session type, we know the other end of this channel
will need to implement:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
# type QuerySum = Session! {
#     loop {
#         choose {
#             0 => send i64,
#             1 => {
#                 recv i64;
#                 break;
#             }
#         }
#     }
# };
type ComputeSum = Session! {
    loop {
        offer {
            0 => recv i64,
            1 => {
                send i64;
                break;
            }
        }
    }
};

assert_type_eq_all!(<QuerySum as Session>::Dual, ComputeSum);
```

We can implement this protocol by following the session types. When the session type of a
[`Chan`] hits the end of a `loop`, it jumps back to the beginning of that loop. In this case,
for example, immediately after the querying task sends an integer, the resultant channel will have
the session type `QuerySum` again.

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# type QuerySum = Session! {
#     loop {
#         choose {
#             0 => send i64,
#             1 => {
#                 recv i64;
#                 break;
#             }
#         }
#     }
# };
# type ComputeSum = Session! {
#     loop {
#         offer {
#             0 => recv i64,
#             1 => {
#                 send i64;
#                 break;
#             }
#         }
#     }
# };
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
#
let (mut c1, mut c2) = QuerySum::channel(|| mpsc::channel(1));

// Sum all the numbers sent over the channel
tokio::spawn(async move {
    let mut sum = 0;
    let c2 = loop {
        c2 = offer!(in c2 {
            0 => {
                let (n, c2) = c2.recv().await?;
                sum += n;
                c2
            },
            1 => break c2,
        })?;
    };
    c2.send(sum).await?.close();
    Ok::<_, mpsc::Error>(())
});

// Send some numbers to be summed
for n in 0..=10 {
    c1 = c1.choose::<0>().await?.send(n).await?;
}

// Get the sum
let (sum, c1) = c1.choose::<1>().await?.recv().await?;
c1.close();
assert_eq!(sum, 55);
# Ok(())
# }
```

**Notice:** Whenever we loop over a channel, the channel itself needs to be declared `mut`. This
is because each channel operation takes the channel as an owned value, returning a new channel.
In order to repeat a loop, we need to re-assign to the channel at the end of the loop so that it
is in scope for the next iteration.

## Nested loops

If the protocol contains nested loops, you can specify which nested loop to continue with using
identical syntax to Rust: *loop labels*. Labeling a loop enables us to `break` or `continue` to
a loop using its label:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
type LabelExample = Session! {
    'outer: loop {
        send i64;
        loop {
            recv bool;
            offer {
                0 => break 'outer,
                1 => continue 'outer,
                2 => break,
                3 => continue,
                4 => send String,
            };
            send bool;
        };
        recv i64;
    }
};
```

# Sequencing and Modularity

The penultimate session type provided by Dialectic is the `call` type, which permits a modular
way of constructing session types and their implementations. At first, you will likely not need
to use `call`; however, as you build larger programs, it makes it a lot easier to split them up
into smaller, independent specifications and components.

So, what does it do? A session type `call P` means "run the session `P`, then once `P` is `Done`,
continue with whatever is next". In smaller protocols, you likely don't need to use `call` to
sequence operations; instead, the idiomatic thing to do is to use `;` to separate sequential parts
of a session.

However, `call` becomes useful when you already have a subroutine that implements *part of* a
session, and you'd like to use it in a larger context without altering it. Imagine, for example,
that you had already written the following:

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
type Query = Session! {
    send String;
    recv String;
};

async fn query(
    question: String,
    chan: mpsc::Chan<Query>
) -> Result<String, mpsc::Error> {
    let chan = chan.send(question).await?;
    let (answer, chan) = chan.recv().await?;
    chan.close();
    Ok(answer)
}
```

Then, at some later point in time, you might realize you need to implement a protocol that makes
several calls to `Query` in a loop. Unfortunately, the function `query` as written doesn't return
a channel as output; instead, it (correctly) closes the channel after finishing the session. Without
[`call`](Chan::call), you would have to modify both the type `Query` and the function `query`,
just to let them be called from a larger context.

Instead of re-writing both the specification and the implementation, we can use `call` to
integrate `query` as a subroutine in a larger protocol, without changing its type or definition.
All we need to do is use the [`call`](Chan::call) method to call it as a subroutine on the
channel.

```
# use dialectic::prelude::*;
# use dialectic_tokio_mpsc as mpsc;
# use static_assertions::assert_type_eq_all;
# type Query = Session! {
#     send String;
#     recv String;
# };
#
# async fn query(
#     question: String,
#     chan: mpsc::Chan<Query>
# ) -> Result<String, mpsc::Error> {
#     let chan = chan.send(question).await?;
#     let (answer, chan) = chan.recv().await?;
#     chan.close();
#     Ok(answer)
# }
#
type MultiQuery = Session! {
    loop {
        choose {
            0 => break,
            1 => call Query,
        }
    }
};

async fn query_all(
    mut questions: Vec<String>,
    mut chan: mpsc::Chan<MultiQuery>
) -> Result<Vec<String>, mpsc::Error> {
    let mut answers = Vec::with_capacity(questions.len());
    for question in questions.into_iter() {
        let (answer, c) =
            chan.choose::<1>().await?
                .call(|c| query(question, c)).await?;  // Call `query` as a subroutine
        chan = c.unwrap();
        answers.push(answer);
    }
    chan.choose::<0>().await?.close();
    Ok(answers)
}
```

Furthermore, [`call`](Chan::call) can be used to implement **context free session types**, where
the sub-protocol `P` uses `continue` to recur back outside the `call` itself. This allows you to
define recursive protocols that can be shaped like any arbitrary tree. For more details, see the
documentation for [`call`](Chan::call) and the
[`stack` example](https://github.com/boltlabs-inc/dialectic/tree/main/examples/stack.rs).

## Errors in subroutines (what not to do)

In order for the [`call`](Chan::call) method to preserve the validity of the session type
`call P; Q`, we need to make sure that the subroutine executing `P` **finishes** the session `P` by
driving the channel to the `Done` session type. If we didn't check this, it would be possible to
drop the channel early in the subroutine, thus allowing steps to be skipped in the protocol. The
[`call`](Chan::call) method solves this problem by returning a pair of the subroutine's return
value and a [`Result`] which is a [`Chan`] for `Q` if `P` was completed successfully, or a
[`SessionIncomplete`] error if not. It's almost always a programmer error if you get a
[`SessionIncomplete`] error, so it's usually the right idea to [`unwrap`](Result::unwrap) it and
proceed without further fanfare.

[`SessionIncomplete`]: crate::SessionIncomplete

💡 **A useful pattern:** If you make sure to *always* call [`close`](Chan::close) on the channel
before the subroutine's future returns, you can be guaranteed that such errors are impossible,
because [`close`](Chan::close) can only be called when a channel's session is complete.

# Splitting off

Traditional presentations of session types do not allow the channel to be used concurrently to
send and receive at the same time. Some protocols, however, can be made more efficient by
executing certain portions of them in parallel.

Dialectic incorporates this option into its type system with the `split` keyword. A channel with
a session type of `split { -> P, <- Q }` can be [`split`](Chan::split) into a send-only end with the
session type `P` and a receive-only end with the session type `Q`, which can then be used
concurrently. Just as in [`call`](Chan::call), once the given closure finishes using the `P` and
`Q` channels until their sessions are both done, [`split`](Chan::split) returns a channel for
whatever session type followed the `split`, which can again be used for both sending and receiving.

In the example below, the two ends of a channel **concurrently swap** a
`Vec<usize>` and a `String`. If this example were run over a network and these values were
large, this could represent a significant reduction in runtime.

```
# use dialectic::prelude::*;
type SwapVecString = Session! {
    split {
        -> send Vec<usize>,
        <- recv String,
    }
};
```

The dual of `split { -> P, <- R }` is `split { -> R::Dual, <- P::Dual }`. Notice that `P` and `Q`
switch places! This is because the `->` side of the `split` is always the send-only session, and the
`<-` side is always the receive-only session:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
assert_type_eq_all!(
    <Session! {
        split {
            -> send Vec<usize>,
            <- recv String,
        }
    } as Session>::Dual,
    Session! {
        split {
            -> send String,
            <- recv Vec<usize>,
        }
    },
);
```

Now, let's use a channel of this session type to enact a concurrent swap of a `String` and a
`Vec<usize>`:

```
use dialectic::prelude::*;
use dialectic_tokio_mpsc as mpsc;

# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
type SwapVecString = Session! {
    split {
        -> send Vec<usize>,
        <- recv String,
    }
};

let (c1, c2) = SwapVecString::channel(|| mpsc::channel(1));

// Spawn a thread to simultaneously send a `Vec<usize>` and receive a `String`:
let t1 = tokio::spawn(async move {
    c1.split(|tx, rx| async move {
        // In one thread we send the `Vec`...
        let send_vec = tokio::spawn(async move {
            tx.send(vec![1, 2, 3, 4, 5]).await?;
            Ok::<_, mpsc::Error>(())
        });
        // In another thread we receive the `String`...
        let recv_string = tokio::spawn(async move {
            let (string, _) = rx.recv().await?;
            Ok::<_, mpsc::Error>(string)
        });
        send_vec.await.unwrap()?;
        let string = recv_string.await.unwrap()?;
        Ok::<_, mpsc::Error>(string)
    }).await
});

// Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*, the task above:
c2.split(|tx, rx| async move {
    // In one thread we send the `String`...
    let send_string = tokio::spawn(async move {
        tx.send("Hello!".to_string()).await?;
        Ok::<_, mpsc::Error>(())
    });
    // In another thread we receive the `Vec`...
    let recv_vec = tokio::spawn(async move {
        let (vec, _) = rx.recv().await?;
        Ok::<_, mpsc::Error>(vec)
    });

    // Examine the result values:
    send_string.await??;
    let vec = recv_vec.await??;
    let string = t1.await??.0;
    assert_eq!(vec, &[1, 2, 3, 4, 5]);
    assert_eq!(string, "Hello!");

    Ok::<_, Box<dyn std::error::Error>>(())
}).await?;
#
# Ok(())
# }
```

When using `split`, keep in mind that it is subject to some limitations in addition to the
caveats of `call`, of which it is a close relative:

- It's a type error to [`send`](Chan::send) or [`choose`](Chan::choose) on the receive-only end.
- It's a type error to [`recv`](Chan::recv) or [`offer`](crate::offer) on the transmit-only end.
- It's a runtime [`SessionIncomplete`] error if you don't drop both the `tx` and `rx` ends
  before the future completes. This is subject to the same behavior as in [`call`](Chan::call),
  described below. [See here for more explanation](#errors-in-subroutines-what-not-to-do).

# Writing backend-polymorphic code

When writing functions which are polymorphic over their backend type, you will need to specify
[`Transmit`] and [`Receive`] bounds on your backend. If you have a lot of these, the
[`macro@Transmitter`] and [`macro@Receiver`] attribute macros can help eliminate them by letting
you write them efficiently. For instance, suppose we have a protocol using the unit structs `T1`,
`T2`, `T3`, `T4`, and `T5`. We can simplify writing bounds for the `run` function using these
attributes.

```
use dialectic::prelude::*;
use std::error::Error;

type S = Session! {
    send T1;
    recv T2;
    send T3;
    recv T4;
    send T5;
};
# struct T1;
# struct T2;
# struct T3;
# struct T4;
# struct T5;

#[Transmitter(Tx for T1, T3, T5)]
#[Receiver(Rx for T2, T4)]
async fn run<Tx, Rx>(
    chan: Chan<S, Tx, Rx>,
) -> Result<(), Box<dyn Error>>
where
    Tx::Error: Error + Send,
    Rx::Error: Error + Send,
{
    let chan = chan.send(T1).await?;
    let (T2, chan) = chan.recv().await?;
    let chan = chan.send(T3).await?;
    let (T4, chan) = chan.recv().await?;
    let chan = chan.send(T5).await?;
    chan.close();
    Ok(())
}
```

For more details on the syntax for the [`macro@Transmitter`] and [`macro@Receiver`] attribute
macros, see their documentation. Additionally, the code in the
[examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples)
is written to be backend-agnostic, and uses these attributes. They may prove an additional resource
if you get stuck.

# Wrapping up

We've now finished our tour of everything you need to get started programming with Dialectic! ✨

You might now want to...

- Check out the documentation for **[`Chan`]**, if you haven't already?
- Learn how to instantiate (or implement) a **[`backend`](crate::backend)** other than the
  **[`mpsc`]** backend we used in the tutorial above?
- Jump back to the top of **[reference documentation](crate#quick-reference)**?

Thanks for following along, and enjoy!

[`Session`]: trait@crate::Session
[`dialectic_tokio_mpsc`]: https://docs.rs/dialectic-tokio-mpsc
[`mpsc`]: https://docs.rs/dialectic-tokio-mpsc
*/

// Import the whole crate so the docs above can link appropriately.
#![allow(unused_imports)]
use crate::prelude::*;
