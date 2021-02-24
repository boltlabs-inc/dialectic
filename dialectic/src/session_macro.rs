/*
The `Session!` macro compiles a small domain-specific language for describing session types into
the types themselves. It should be invoked in type position.

The `Session!` macro language looks a lot like Rust code: sequential instructions are separated
by semicolons, blocks are delimited with curly braces, etc.

The language defines several keywords, each of which corresponding to the session type of the
same name (except for `break`, which is translated by the macro into other constructs):

- [`send` and `recv`](#the-send-and-recv-keywords)
- [`offer` and `choose`](#the-offer-and-choose-keywords)
- [`loop`, `break`, and `continue`](#the-loop-break-and-continue-keywords)
- [`call`](#the-call-keyword)
- [`split`](#the-split-keyword)

Additionally, [arbitrary session types can be used inline by name](#external-session-types), so
specifications can be composed.

In the below examples, all code blocks import:

```
use static_assertions::assert_type_eq_all as type_eq;
use dialectic::prelude::*;
```

# The `send` and `recv` keywords

These keywords take the type to be sent or received as an argument, with no parentheses. See
also: the [`Send`] type and [`send`] method, and the [`Recv`] type and [`recv`] method.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
#
type_eq!(
    Session! { send bool },
    Send<bool, Done>,
);

type_eq!(
    Session! { recv bool },
    Recv<bool, Done>,
);
```

# The `offer` and `choose` keywords

Using `offer` and `choose` matches the syntax of the [`offer!`](crate::offer) macro, with each
numerically-labeled branch listed in order, corresponding to the session type to be used in the
case of each potential choice.

See also: the [`Choose`] type and the [`choose`] method, and the [`Offer`] type and the
[`offer`] macro.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
#
type_eq!(
    Session! { offer { _0 => {}, _1 => {} } },
    Offer<(Done, Done)>
);

type_eq!(
    Session! { choose { _0 => {}, _1 => {} } },
    Choose<(Done, Done)>
);
```

# The `loop`, `break`, and `continue` keywords

The syntax of `loop`, `break` and `continue` are identical to in Rust syntax, including optional
named labels.

Just like Rust's `loop` keyword, but *unlike* Dialectic's [`Loop`] type, when control flow
reaches the end of a `loop` in a `Session!`, it automatically returns to the head of the loop.
To exit a loop, you must use `break`.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
#
type_eq!(
    Session! { loop { break } },
    Loop<Done>
);

type_eq!(
    Session! { loop { send (); } },
    Session! { loop { send (); continue; } },
    Loop<Send<(), Continue>>,
)

type_eq!(
    Session! {
        'outer: loop {
            loop {
                break 'outer;
            }
        }
    },
    Loop<Loop<Done>>
);

type_eq!(
    Session! {
        'outer: loop {
            send ();
            loop {
                continue 'outer;
            }
        }
    },
    Loop<Send<(), Loop<Continue<_1>>>>
);
```

TODO: document call, split, and the use of arbitrary types

# External session types

Arbitrary session types defined outside the macro invocation can be spliced in as an individual
statement, or even used as generic parameters.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
#
type Parity = Session! { send i64; recv bool };
type Twice<T> = Session! { T; T };
type Protocol = Session! {
    loop {
        choose {
            _0 => Twice<Parity>,
            _1 => break,
        }
    }
};

type_eq!(
    Protocol,
    Loop<Choose<(Send<i64, Recv<bool, Send<i64, Recv<bool, Continue>>>>, Done)>>,
);
```

# Examples

In these examples, each example of a session type defined using `Session!` is paired with its
equivalent session type written out using the session type constructors in
[`types`](crate::types).

You can find further examples of the use of `Session!` and its meaning throughout this reference
documentation, the [`tutorial`](crate::tutorial), and the
[examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples).

```
use static_assertions::assert_type_eq_all;
use dialectic::prelude::*;

assert_type_eq_all!(
    Session! {
        send i64;
        recv bool;
    },
    Send<i64, Recv<bool, Done>>,
);

assert_type_eq_all!(
    Session! {
        loop {
            offer {
                _0 => send i64,
                _1 => {
                    recv bool;
                    break;
                }
            }
        }
    },
    Loop<Offer<(Send<i64, Continue>, Recv<bool, Done>)>>,
);
```
*/
pub use dialectic_macro::Session;
