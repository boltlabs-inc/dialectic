#![forbid(broken_intra_doc_links)]

extern crate proc_macro;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::env;
use syn::{
    braced, parse::Parse, parse::ParseStream, parse_macro_input, parse_quote, spanned::Spanned,
    Arm, Ident, Pat, Path, Token,
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
pub fn Session(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let result = parse_macro_input!(input as dialectic_compiler::Invocation).compile();

    match result {
        Ok(compiled) => compiled.to_token_stream().into(),
        Err(error) => {
            let compile_errors = error.to_compile_error();
            let quoted = quote! { [(); { #compile_errors 0 }] };
            quoted.into()
        }
    }
}

/// The `offer!` macro offers a set of different protocols, allowing the other side of the channel
/// to choose with which one to proceed.
///
/// Each invocation of the `offer!` macro on some `Chan<S, Tx, Rx>` returns `Result<_, Rx::Error>`,
/// which will be an error if the receiving transport end `Rx` failed to receive the choice made by
/// the other party. Otherwise, it returns whatever type is returned by each of its branches (which
/// means that each branch must have the same type).
///
/// You must specify exactly as many branches as there are options in the type of the [`Offer`] to
/// which this expression corresponds, and they must be in the same order as the choices are in the
/// tuple [`Offer`]ed. In the body of each branch, the identifier for the channel is rebound to have
/// the session type corresponding to that branch, so you can proceed with its session.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
/// use dialectic::backend::mpsc;
///
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
/// type GiveOrTake = Session! {
///     choose {
///         _0 => send i64,
///         _1 => recv String,
///     }
/// };
///
/// let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
///
/// // Spawn a thread to offer a choice
/// let t1 = tokio::spawn(async move {
///     offer!(c2 => {
///         _0 => { c2.recv().await?; },
///         _1 => { c2.send("Hello!".to_string()).await?; },
///     })?;
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
///
/// [`Offer`]: https://docs.rs/dialectic/latest/dialectic/types/struct.Offer.html
#[proc_macro]
pub fn offer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let result = parse_macro_input!(input as OfferInvocation).compile();
    match result {
        Ok(output) => output.to_token_stream().into(),
        Err(error) => error.to_compile_error().into(),
    }
}

/// The surface syntax of the `offer!` macro, as captured by `syn`'s parsing machinery.
struct OfferInvocation {
    /// The identifier of the channel to be offered upon.
    chan: syn::Ident,
    /// The syntactic branches of the invocation: this could be invalid, because [`Arm`] contains
    /// many variants for its patterns which are not legitimate in our context.
    branches: Vec<Arm>,
}

/// The output information generated by the `offer!` macro, as required to quote it back into a
/// generated [`TokenStream`].
struct OfferOutput {
    /// The identifier of the channel to be offered upon.
    chan: syn::Ident,
    /// The branches, in order, to be emitted for the `match` statement generated by the macro.
    branches: Vec<syn::Expr>,
}

impl Parse for OfferInvocation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let chan = input.parse::<Ident>()?;
        let _ = input.parse::<Token![=>]>()?;
        let content;
        let _ = braced!(content in input);
        let mut branches = Vec::new();
        while !content.is_empty() {
            branches.push(content.call(Arm::parse)?);
        }
        Ok(OfferInvocation { chan, branches })
    }
}

impl ToTokens for OfferOutput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use proc_macro_crate::FoundCrate;

        let OfferOutput { chan, branches } = self;

        // We need to find the right path where we can reference types in our proc macro. This is a
        // little tricky. There are three cases to consider.
        let dialectic_crate: Path = match proc_macro_crate::crate_name("dialectic") {
            // The first case is that we are in dialectic and compiling dialectic itself, OR we are
            // compiling a dialectic doctest. In this case, we want to use `crate::dialectic`, which
            // will grab the symbol "dialectic" in the crate root. In the case of a doctest, this
            // will result in the extern crate dialectic; in the case of dialectic itself, it will
            // result in a private dummy module called "dialectic", which exists to support macro
            // calls like these and re-exports dialectic::types.
            Ok(FoundCrate::Itself)
                if env::var("CARGO_CRATE_NAME").as_deref() == Ok("dialectic") =>
            {
                parse_quote!(crate::dialectic)
            }
            // The second case is that we are in an integration test of dialectic. This one's
            // straightforward.
            Ok(FoundCrate::Itself) | Err(_) => parse_quote!(::dialectic),
            // And lastly, the third case: we are in a user's crate. We found the crate with the
            // name `dialectic` and will use that identifier as our crate name, in a similar manner
            // to the second case, prefixed with `::` to ensure it is a "global" path.
            Ok(FoundCrate::Name(name)) => {
                let name_ident = format_ident!("{}", name);
                parse_quote!(::#name_ident)
            }
        };

        // Convert a `usize` into the tokens that represent it in unary
        let unary = |n: usize| -> TokenStream {
            let mut output = quote! { #dialectic_crate::unary::Z };
            for _ in 0..n {
                output = quote! { #dialectic_crate::unary::S( #output ) };
            }
            output
        };

        // Create an iterator of token streams, one for each branch of the generated `match`
        let arms = branches.iter().enumerate().map(|(choice, body)| {
            let n = unary(choice);
            let choice = choice as u8;
            quote! {
                #choice => {
                    let #chan = #dialectic_crate::Branches::case(#chan, #n);
                    let #chan = match #chan {
                        Ok(chan) => chan,
                        Err(_) => unreachable!("malformed generated code from `offer!` macro: mismatch between variant and choice: this is a bug!"),
                    };
                    #body
                }
            }
        });

        // Merge all the branches into one `match` statement that branches precisely once, on the
        // discriminant of the `Branches` structure
        tokens.append_all(quote! {
            {
                match #dialectic_crate::Chan::offer( #chan ).await {
                    Ok(#chan) => Ok(match {
                            let choice: u8 = std::convert::Into::into(dialectic::Branches::choice(& #chan));
                            choice
                        } {
                            #(#arms),*
                            _ => unreachable!("malformed code generated from `offer!` macro: impossible variant encountered: this is a bug!"),
                        }),
                    Err(error) => Err(error),
                }
            }
        })
    }
}

impl OfferInvocation {
    /// Compile an [`OfferInvocation`] to an [`OfferOutput`], or return an error if it was not a
    /// valid macro invocation.
    fn compile(self) -> Result<OfferOutput, syn::Error> {
        // Validate the structure, collecting all errors
        let mut errors: Vec<syn::Error> = Vec::new();
        for (choice, arm) in self.branches.iter().enumerate() {
            if choice > 128 {
                let message = format!("at most 128 arms (labeled `_0` through `_127` in ascending order) are permitted in the `offer!` macro; this arm is number {}", choice);
                errors.push(syn::Error::new(arm.span(), message));
            }
            match &arm.pat {
                Pat::Ident(pat_ident) => {
                    if let Some((_, ref guard)) = arm.guard {
                        let message = "guards are not permitted in patterns for the `offer!` macro";
                        errors.push(syn::Error::new(guard.span(), message))
                    }
                    if let Some(by_ref) = pat_ident.by_ref {
                        let message = "`ref` annotations are not permitted in patterns for the `offer!` macro";
                        errors.push(syn::Error::new(by_ref.span(), message))
                    }
                    if let Some(mutability) = pat_ident.mutability {
                        let message = "`mut` annotations are not permitted in patterns for the `offer!` macro";
                        errors.push(syn::Error::new(mutability.span(), message))
                    }
                    if let Some((_, ref subpat)) = pat_ident.subpat {
                        let message =
                            "sub-patterns using `@` are not permitted in patterns for the `offer!` macro";
                        errors.push(syn::Error::new(subpat.span(), message))
                    }
                    if pat_ident.ident != format!("_{}", choice) {
                        let message = format!("expected the identifier `_{}` for this arm of the `offer!` macro (note: arms must be in ascending order)", choice);
                        errors.push(syn::Error::new(pat_ident.ident.span(), message))
                    }
                }
                _ => {
                    let message = format!("expected the identifier `_{}` for this arm (note: arms must be in ascending order)", choice);
                    errors.push(syn::Error::new(arm.pat.span(), message))
                }
            }
        }

        // Either collect the valid expressions, one for each branch, or return the errors
        match errors.pop() {
            None => Ok(OfferOutput {
                chan: self.chan,
                branches: self
                    .branches
                    .into_iter()
                    .map(|Arm { body, .. }| *body)
                    .collect(),
            }),
            Some(mut error) => {
                for e in errors.drain(..) {
                    error.combine(e)
                }
                Err(error)
            }
        }
    }
}
