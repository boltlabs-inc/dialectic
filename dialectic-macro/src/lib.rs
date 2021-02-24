#![forbid(broken_intra_doc_links)]
extern crate proc_macro;
use {
    proc_macro::TokenStream,
    quote::{quote, ToTokens},
    syn::parse_macro_input,
};

/**
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

# Examples

In these examples, each example of a session type defined using `Session!` is paired with its
equivalent session type written out using the session type constructors in
[`types`](https://docs.rs/dialectic/latest/dialectic/types.index.html).

You can find further examples of the use of `Session!` and its meaning throughout this reference
documentation, the [`tutorial`](https://docs.rs/dialectic/latest/dialectic/tutorial.index.html), and
[in the Dialectic crate's examples](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic/examples).

In the below examples, all code blocks import:

```
use static_assertions::assert_type_eq_all as type_eq;
use dialectic::prelude::*;
```

## The `send` and `recv` keywords

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

## The `offer` and `choose` keywords

Using `offer` and `choose` matches the syntax of the [`offer!`] macro, with each
numerically-labeled branch listed in order, corresponding to the session type to be used in the
case of each potential choice.

See also: the [`Choose`] type and the [`choose`] method, and the [`Offer`] type
and the [`offer!`] macro.

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

## The `loop`, `break`, and `continue` keywords

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
);

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

## The `call` keyword

The `call` keyword either precedes a named session type, or a block. It corresponds to the
[`Call`] session type and the [`call`] method.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
#
type P = Session! { send i64 };
type Q = Session! { call P };

type_eq!(Q, Call<Send<i64, Done>, Done>);

type R = Session! {
    loop {
        choose {
            _0 => {
                send i64;
                call { continue };
                recv i64;
            },
            _1 => break,
        }
    }
};

type_eq!(R, Loop<Choose<(Send<i64, Call<Continue, Recv<i64, Continue>>>, Done)>>);
```

## The `split` keyword

The `split` keyword precedes a block with two clauses, indicating two concurrent sessions to be
run, one (marked by `->`) which can only transmit information, and the other (marked by `<-`)
which can only receive information. It corresponds to the [`Split`] session type and the [`split`]
method.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
#
type P = Session! {
    split {
        -> send i64,
        <- recv bool,
    };
    send String;
};

type_eq!(P, Split<Send<i64, Done>, Recv<bool, Done>, Send<String, Done>>);
```

## External session types

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

[`Send`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Send.html
[`Recv`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Recv.html
[`Offer`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Offer.html
[`Choose`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Choose.html
[`Loop`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Loop.html
[`Continue`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Continue.html
[`Done`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Done.html
[`Call`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Call.html
[`Split`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Split.html
[`send`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html#method.send
[`recv`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html#method.recv
[`offer!`]: https://docs.rs/dialectic/latest/dialectic/macro.offer.html
[`choose`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html#method.choose
[`call`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html#method.call
[`split`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html#method.split
*/
#[proc_macro]
#[allow(non_snake_case)]
pub fn Session(input: TokenStream) -> TokenStream {
    let result = parse_macro_input!(input as dialectic_compiler::Invocation)
        .syntax
        .to_session();

    match result {
        Ok(compiled) => compiled.to_token_stream().into(),
        Err(error) => {
            let compile_errors = error.to_compile_error();
            let quoted = quote! { [(); { #compile_errors 0 }] };
            quoted.into()
        }
    }
}
