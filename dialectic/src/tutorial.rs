/*! The introductory tutorial for Dialectic (nothing is exported from this module).

# Getting started with Dialectic

The first step to communicating is figuring out what you mean to say.

This crate provides a concise [type-level language for expressing session types](crate::types).
The session type attached to a wrapped communications channel indicates precisely which actions
are available for each end of the channel.

Let's write our first session type:

```
use dialectic::prelude::*;

type JustSendOneString = Send<String, Done>;
```

This type specifies a very simple protocol: "send a string, then finish." The first argument to
`Send` is the type sent, and the second is the rest of the protocol, which in this case is the
empty protocol [`Done`].

While the types needed to express simple session types like the above are easy enough to read,
they can become quite difficult to parse for more complex sessions. For this reason, Dialectic
has the [`Session!`](crate::Session@macro) macro, which defines a simple domain specific language
for specifying session types. We'll use both the `Session!` macro and the raw session types in
this tutorial, to help the reader understand the relationship between the two.

```
# use dialectic::prelude::*;
type JustSendOneString = Session! {
    send String;
};
```

Every session type has a [`Dual`](crate::Session::Dual), which describes what the other end of
the channel must do to follow the channel's protocol; if one end [`Send`]s, the other end must
[`Recv`]:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
assert_type_eq_all!(<Send<String, Done> as Session>::Dual, Recv<String, Done>);
```

And now, expressing the same thing in terms of the `Session!` macro:

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

In this case, we're using the [`mpsc`](crate::backend::mpsc) backend Dialectic provides, which
is built on [`tokio::sync::mpsc`]. However, the mechanism for wrapping underlying channels is
extensible, meaning you can choose your own transport if you want.

```
# use dialectic::prelude::*;
use dialectic::backend::mpsc;
```

The static method [`channel`](crate::Session::channel) is automatically defined for all valid
session types (note: its corresponding trait [`Session`] needs to be in scope for it to be
callable). It takes as input a closure that creates some underlying unidirectional transport
channel, and creates a matched pair of bidirectional session-typed channels wrapping the
underlying channel type.

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
```

The types of `c1` and `c2` are inferred from the session type specified: `c1`'s type corresponds
to the given session type, and `c2`'s type corresponds to its dual:

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
# let (c1, c2) = JustSendOneString::channel(|| mpsc::channel(1));
let _: Chan<Session! { send String }, mpsc::Sender, mpsc::Receiver> = c1;
let _: Chan<Session! { recv String }, mpsc::Sender, mpsc::Receiver> = c2;
```

Now that we have the two ends of a bidirectional session-typed channel, we can use them to
concurrently enact the protocol specified by their type. In this case, we're going to run them
in two parallel [`tokio`] tasks. However, Dialectic is generic over the underlying async
runtime, provided the underlying transport channel is of a compatible type.

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
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
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
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

Trying to do any of the below things results in a compile-time type error (edited for brevity).

If we try to send on the end of the channel that's meant to receive...

```compile_fail
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
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
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
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
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
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

Most interesting protocols don't just consist of linear sequences of [`Send`]s and [`Recv`]s.
Sometimes, one party offers the other a choice of different ways to proceed, and the other
chooses which path to take.

In Dialectic, this possibility is represented by the [`Offer`] and [`Choose`] types. Each is
parameterized by a *tuple* of session types representing, respectively, the choices offered by
one end of the channel, or the choices available to choose from.

`Offer` is dual to `Choose` if the choices offered are dual to the choices available to choose
from:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
type P = Offer<(Send<String, Done>, Recv<i64, Done>)>;
type Q = Choose<(Recv<String, Done>, Send<i64, Done>)>;

assert_type_eq_all!( <P as Session>::Dual, Q);
```

To express these types in the [`Session!`](crate::Session@macro) macro, we can use the `offer`
and `choose` keywords:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
type P = Session! {
    offer {
        _0 => send String,
        _1 => recv i64,
    }
};

type Q = Session! {
    choose {
        _0 => recv String,
        _1 => send i64,
    }
};

assert_type_eq_all!( <P as Session>::Dual, Q);
```

Just as the [`send`](Chan::send) and [`recv`](Chan::recv) methods enact the [`Send`] and
[`Recv`] session types, the [`choose`](Chan::choose) method and [`offer!`](crate::offer) macro
enact the [`Choose`] and [`Offer`] session types.

Suppose we want to offer a choice between two protocols: either sending a single integer
(`Send<i64, Done>`) or receiving a string (`Recv<String, Done>`). Correspondingly, the other end
of the channel must indicate a choice of which protocol to follow, and we need to handle the
result of either selection by enacting the protocol chosen.

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type JustSendOneString = Send<String, Done>;
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {

type P = Session! {
    offer {
        _0 => send i64,
        _1 => recv String,
    }
};

let (c1, c2) = P::channel(|| mpsc::channel(1));

// Offer a choice
let t1 = tokio::spawn(async move {
    let c1 = offer!(c1 => {
        _0 => c1.send(42).await?,  // handle `c2.choose(_0)`
        _1 => c1.recv().await?.1,  // handle `c2.choose(_1)`
    });
#   c1.close();
    Ok::<_, mpsc::Error>(())
});

// Make a choice
let t2 = tokio::spawn(async move {
    let c2 = c2.choose(_1).await?;            // select to `Send<String, Done>`
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
channel, passing it as input a constant corresponding to the index of the choice. These
constants are *not* Rust's built-in numeric types, but rather [unary type-level
numbers](crate::unary). Dialectic supports up to 128 possible choices in an `Offer` or `Choose`,
and the corresponding constants [`_0`](crate::unary::constants::_0),
[`_1`](crate::unary::constants::_1), [`_2`](crate::unary::constants::_2), ...
[`_127`](crate::unary::constants::_127) are defined in the
[`constants`](crate::unary::constants) module.

# Looping back

While some protocols can be described as a finite sequence of operations, many contain the
possibility of loops. Consider things like retrying after an invalid response, sending an
unbounded stream of messages, or providing a persistent connection for multiple queries. All
these can be modeled with session types by introducing *recursion*.

Suppose we want to send a stream of as many integers as desired, then receive back their sum. We
could describe this protocol using the [`Loop`] and [`Continue`] types:

```
# use dialectic::prelude::*;
type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64, Done>)>>;
```

In terms of the [`Session!`](crate::Session@macro) macro, this would be:

```
# use dialectic::prelude::*;
type QuerySum = Session! {
    loop {
        choose {
            _0 => send i64,
            _1 => {
                recv i64;
                break;
            }
        }
    }
};
```

**Notice:** When working with session types directly, we have to *explicitly* use [`Continue`]
to reiterate the [`Loop`]: even inside a [`Loop`], [`Done`] means that the session is over.
However, in the [`Session!`](crate::Session@macro) macro, `loop`s repeat themselves by default,
unless explicitly broken by a `break` statement (just like in Rust!).

By taking the dual of each part of the session type, we know the other end of this channel
will need to implement:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
# type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64, Done>)>>;
type ComputeSum = Session! {
    loop {
        offer {
            _0 => recv i64,
            _1 => {
                send i64;
                break;
            }
        }
    }
};

assert_type_eq_all!(<QuerySum as Session>::Dual, ComputeSum);
```

We can implement this protocol by following the session types. When the session type of a
[`Chan`] hits a [`Continue`] point, it jumps back to the type of the [`Loop`] to which that
[`Continue`] refers. In this case, for example, immediately after the querying task sends an
integer, the resultant channel will have the session type `Choose<(Send<i64, Continue>,
Recv<i64, Done>)>` once more.

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type QuerySum = Loop<Choose<(Send<i64, Continue>, Recv<i64, Done>)>>;
# type ComputeSum = Loop<Offer<(Recv<i64, Continue>, Send<i64, Done>)>>;
# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
#
let (mut c1, mut c2) = QuerySum::channel(|| mpsc::channel(1));

// Sum all the numbers sent over the channel
tokio::spawn(async move {
    let mut sum = 0;
    let c2 = loop {
        c2 = offer!(c2 => {
            _0 => {
                let (n, c2) = c2.recv().await?;
                sum += n;
                c2
            },
            _1 => break c2,
        });
    };
    c2.send(sum).await?.close();
    Ok::<_, mpsc::Error>(())
});

// Send some numbers to be summed
for n in 0..=10 {
    c1 = c1.choose(_0).await?.send(n).await?;
}

// Get the sum
let (sum, c1) = c1.choose(_1).await?.recv().await?;
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

If the protocol contains nested loops, you can specify which nested loop to continue with  using
the optional parameter of `Continue`. By default, `Continue` jumps to the innermost loop;
however, `Continue<_1>` jumps to the second-innermost, `Continue<_2>` the third-innermost, etc.
The types [`_0`](type@_0), [`_1`](type@_1), [`_2`](type@_2), etc. are imported by default in the
[`dialectic::prelude::*`](crate::prelude) namespace.

In the [`Session!`](crate::Session@macro) macro, loops may optionally be *lableled*, using
identical syntax to Rust. Rather than referring to loops by index, we can `break` or `continue`
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
                _0 => break 'outer,
                _1 => continue 'outer,
                _2 => break,
                _3 => continue,
                _4 => send String,
            };
            send bool;
        };
        recv i64;
    }
};

assert_type_eq_all!(
    LabelExample,
    Loop<Send<i64, Loop<Recv<bool, Offer<(Done, Continue<_1>, Recv<i64, Continue<_1>>, Continue, Send<String, Send<bool, Continue>>)>>>>>,
);
```

## Automatic looping

You may have noticed how in the example above, [`choose`](Chan::choose) can be called on `c1`
even though the outermost part of `c1`'s session type `QuerySum` would seem not to begin with
[`Choose`]. This is true in general: if the session type of a [`Chan`] is a [`Loop`] whose
internal session type would make a given operation valid, that operation is valid on the
[`Chan`]. In the instance above, calling [`choose`](Chan::choose) on a [`Chan`] with session
type `Loop<Choose<...>>` works, no matter how many `Loop`s enclose the `Choose`.

This behavior is enabled by the [`Session`] trait, which in addition to defining the
[`Dual`](crate::Session::Dual) of each session type, defines what the next "real"
[`Action`](crate::Session::Action) on a session type is. For most session types, the "real
action" is that session type itself. However, for [`Loop`], the next real action is whatever
follows entering the loop(s), with the loop body unrolled by one iteration.

# Sequencing and Modularity

The final session type provided by Dialectic is the [`Call`] type, which permits a more modular
way of constructing session types and their implementations. At first, you will likely not need
to use [`Call`]; however, as you build larger programs, it makes it a lot easier to split them up
into smaller, independent specifications and components.

So, what does it do? A session type `Call<P, Q>` means "run the session `P`, then once `P` is `Done`, run the session `Q`". You can think of it like the `;` in Rust, which concatenates separate statements. In smaller protocols, you don't need to use [`Call`] to sequence operations, because all the session type primitives (with the exception of [`Split`]) take arguments indicating "what to do next": we don't need to write `Call<Send<String, Done>, Recv<String, Done>>`, because `Send<String, Recv<String, Done>>` works just as well.

[`Call`] becomes useful however when you already have a subroutine that implements *part of* a
session, and you'd like to use it in a larger context. Imagine, for example, that you had
already written the following:

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
type Query = Send<String, Recv<String, Done>>;

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
several calls to `Query` in a loop. Unfortunately, the type of `Query` ends with [`Done`], and
its implementation `query` (correctly) closes the [`Chan`] at the end. Without
[`call`](Chan::call), you would have to modify both the type `Query` and the function `query`,
just to let them be called from a larger context.

Instead of re-writing both the specification and the implementation, we can use [`Call`] to
integrate `query` as a subroutine in a larger protocol, without changing its type or definition.
All we need to do is use the [`call`](Chan::call) method to call it as a subroutine on the
channel.

```
# use dialectic::prelude::*;
# use dialectic::backend::mpsc;
# type Query = Send<String, Recv<String, Done>>;
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
type MultiQuery = Loop<Choose<(Done, Call<Query, Continue>)>>;

async fn query_all(
    mut questions: Vec<String>,
    mut chan: mpsc::Chan<MultiQuery>
) -> Result<Vec<String>, mpsc::Error> {
    let mut answers = Vec::with_capacity(questions.len());
    for question in questions.into_iter() {
        let (answer, c) =
            chan.choose(_1).await?
                .call(|c| query(question, c)).await?;  // Call `query` as a subroutine
        chan = c.unwrap();
        answers.push(answer);
    }
    chan.choose(_0).await?.close();
    Ok(answers)
}
```

Furthermore, [`call`](Chan::call) can be used to implement **context free session types**, where
the sub-protocol in the first half `P` of `Call<P, Q>` uses [`Continue`] to recur back outside
the [`Call`] itself. This allows you to define recursive protocols that can be shaped like any
arbitrary tree. For more details, see the documentation for [`call`](Chan::call) and the [`stack` example](https://github.com/boltlabs-inc/dialectic/tree/main/examples/stack.rs).

## Errors in subroutines (what not to do)

In order for the [`call`](Chan::call) method to preserve the validity of the session type `Call<P,
Q>`, we need to make sure that the subroutine executing `P` **finishes** the session `P` by
driving the channel to the `Done` session type. If we didn't check this, it would be possible to
drop the channel early in the subroutine, thus allowing steps to be skipped in the protocol. The
[`call`](Chan::call) method solves this problem by returning a pair of the subroutine's return
value and a [`Result`] which is a [`Chan`] for `Q` if `P` was completed successfully, or a
[`SessionIncomplete`] error if not. It's almost always a programmer error if you get a
[`SessionIncomplete`] error, so it's usually the right idea to [`unwrap`](Result::unwrap) it and
proceed without further fanfare.

💡 **A useful pattern:** If you make sure to *always* call [`close`](Chan::close) on the channel
before the subroutine's future returns, you can be guaranteed that such errors are impossible,
because [`close`](Chan::close) can only be called when a channel's session is complete.

# Splitting off

Traditional presentations of session types do not allow the channel to be used concurrently to
send and receive at the same time. Some protocols, however, can be made more efficient by
executing certain portions of them in parallel.

Dialectic incorporates this option into its type system with the [`Split`] type. A channel with
a session type of `Split<P, Q, R>` can be [`split`](Chan::split) into a send-only end with the
session type `P` and a receive-only end with the session type `Q`, which can then be used
concurrently. Just as in [`call`](Chan::call), once the given closure finishes using the `P` and
`Q` channels until their sessions are both [`Done`], [`split`](Chan::split) returns a channel for
the session type `R`, which can be used for both sending and receiving.

In the example below, the two ends of a channel **concurrently swap** a
`Vec<usize>` and a `String`. If this example were run over a network and these values were
large, this could represent a significant reduction in runtime.

```
# use dialectic::prelude::*;
type SwapVecString = Split<Send<Vec<usize>, Done>, Recv<String, Done>, Done>;
```

The dual of `Split<P, Q, R>` is `Split<Q::Dual, P::Dual, R::Dual>`:

```
# use dialectic::prelude::*;
# use static_assertions::assert_type_eq_all;
assert_type_eq_all!(
    <Split<Send<Vec<usize>, Done>, Recv<String, Done>, Done> as Session>::Dual,
    Split<Send<String, Done>, Recv<Vec<usize>, Done>, Done>,
);
```

Notice that `P` and `Q` switch places! This is because the left-hand `P` is always the send-only
session, and the right-hand `Q` is always the receive-only session.

Now, let's use a channel of this session type to enact a concurrent swap of a `String` and a
`Vec<usize>`:

```
use dialectic::prelude::*;
use dialectic::backend::mpsc;

# #[tokio::main]
# async fn main() -> Result<(), Box<dyn std::error::Error>> {
type SendAndRecv = Split<Send<Vec<usize>, Done>, Recv<String, Done>, Done>;

let (c1, c2) = SendAndRecv::channel(|| mpsc::channel(1));

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

// Simultaneously *receive* a `Vec<usize>` *from*, and *send* a `String` *to*,
// the task above:
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

When using [`Split`], keep in mind that it is subject to some limitations, in addition to the
caveats of [`Call`], of which it is a close relative:

- It's a type error to [`Send`] or [`Choose`] on the receive-only end.
- It's a type error to [`Recv`] or [`Offer`] on the transmit-only end.
- It's a runtime [`SessionIncomplete`] error if you don't drop both the `tx` and `rx` ends
  before the future completes. This is subject to the same behavior as in [`call`](Chan::call),
  described below. [See here for more explanation](#errors-in-subroutines-what-not-to-do).

# Wrapping up

We've now finished our tour of everything you need to get started programming with Dialectic! ✨

You might now want to...

- Check out the documentation for **[`Chan`]**, if you haven't already?
- Learn how to instantiate (or implement) a **[`backend`](crate::backend)** other than the
  **[`mpsc`](crate::backend::mpsc)** backend we used in the tutorial above?
- Jump back to the top of **[reference documentation](crate#quick-reference)**?

Thanks for following along, and enjoy!

[`Session`]: trait@crate::Session
*/

// Import the whole crate so the docs above can link appropriately.
#![allow(unused_imports)]
use crate::prelude::*;
