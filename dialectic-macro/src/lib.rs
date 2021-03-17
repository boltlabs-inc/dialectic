#![forbid(rustdoc::broken_intra_doc_links)]

extern crate proc_macro;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{
    braced, parse::Parse, parse::ParseStream, parse_macro_input, spanned::Spanned, Arm, Ident,
    LitInt, Pat, Token,
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

// Normally you don't need to import these, because they are only useful
// when writing session types directly, not when using the `Session!` macro:
use dialectic::types::*;
use dialectic::unary::types::*;
```

## The `send` and `recv` keywords

These keywords take the type to be sent or received as an argument, with no parentheses. See
also: the [`Send`] type and [`send`] method, and the [`Recv`] type and [`recv`] method.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
# use dialectic::types::*;
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
# use dialectic::types::*;
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
# use dialectic::types::*;
# use dialectic::unary::types::*;
#
type_eq!(
    Session! { loop { break } },
    Loop<Done>
);

type_eq!(
    Session! { loop { send (); } },
    Session! { loop { send (); continue; } },
    Loop<Send<(), Continue<0>>>,
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
    Loop<Send<(), Loop<Continue<1>>>>
);
```

## The `call` keyword

The `call` keyword either precedes a named session type, or a block. It corresponds to the
[`Call`] session type and the [`call`] method.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
# use dialectic::types::*;
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

type_eq!(R, Loop<Choose<(Send<i64, Call<Continue<0>, Recv<i64, Continue<0>>>>, Done)>>);
```

## The `split` keyword

The `split` keyword precedes a block with two clauses, indicating two concurrent sessions to be
run, one (marked by `->`) which can only transmit information, and the other (marked by `<-`)
which can only receive information. It corresponds to the [`Split`] session type and the [`split`]
method.

```
# use static_assertions::assert_type_eq_all as type_eq;
# use dialectic::prelude::*;
# use dialectic::types::*;
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
# use dialectic::types::*;
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
    Loop<Choose<(Send<i64, Recv<bool, Send<i64, Recv<bool, Continue<0>>>>>, Done)>>,
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
/// use dialectic_tokio_mpsc as mpsc;
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
/// c1.choose::<0>().await?.send(42).await?;
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
        let OfferOutput { chan, branches } = self;

        // Find the path necessary to refer to types in the dialectic crate.
        let dialectic_crate = dialectic_compiler::dialectic_path();

        // Create an iterator of token streams, one for each branch of the generated `match`
        let arms = branches.iter().enumerate().map(|(choice, body)| {
            let byte = choice as u8;
            quote! {
                #byte => {
                    let #chan = #dialectic_crate::Branches::case::<#choice>(#chan);
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

/// **Internal implementation detail:** This proc macro generates implementations of `Tuple` and
/// `AsList` for tuples up to the arity passed in as the argument.
#[proc_macro]
pub fn impl_tuples(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let idents = (0..=arity_limit)
        .map(|i| format_ident!("T{}", i))
        .collect::<Vec<_>>();
    let mut impls = TokenStream::new();

    for i in 0..=arity_limit {
        let ident_slice = &idents[..i];

        let typarams = if ident_slice.is_empty() {
            quote!()
        } else {
            quote!(<#(#ident_slice,)*>)
        };

        let as_tuple = quote!((#(#ident_slice,)*));
        let as_list = ident_slice
            .iter()
            .rev()
            .fold(quote!(()), |acc, ident| quote!((#ident, #acc)));

        let current_impl = quote! {
            impl #typarams Tuple for #as_tuple {
                type AsList = #as_list;
            }

            impl #typarams List for #as_list {
                type AsTuple = #as_tuple;
            }
        };

        current_impl.to_tokens(&mut impls);
    }

    impls.into()
}

/// **Internal implementation detail:** This proc macro generates type synonyms for the
/// dialectic::unary::types module. It will generate up to the maximum number specified as the
/// argument.
#[proc_macro]
pub fn generate_unary_types(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let type_idents = (0..=arity_limit).map(|i| format_ident!("_{}", i));
    let type_values = type_idents.fold(vec![], |mut acc, ident| {
        let value = match acc.last() {
            Some((pred_ident, _)) => quote!(S<#pred_ident>),
            None => quote!(Z),
        };

        acc.push((ident, value));

        acc
    });

    let types = type_values.iter().map(|(ident, value)| {
        quote!(
            pub type #ident = #value;
        )
    });

    let contents = quote! {
        use crate::unary::{S, Z};

        #(#types)*
    };

    contents.into()
}

/// **Internal implementation detail:** This proc macro generates unary type constants for the
/// dialectic::unary::constants module. It will generate up to the maximum number specified as the
/// argument.
#[proc_macro]
pub fn generate_unary_constants(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let constant_idents = (0..=arity_limit).map(|i| format_ident!("_{}", i));
    let constant_values = constant_idents.fold(vec![], |mut acc, ident| {
        let value = match acc.last() {
            Some((pred_ident, _)) => quote!(S(#pred_ident)),
            None => quote!(Z),
        };

        acc.push((ident, value));

        acc
    });

    let constants = constant_values.iter().map(|(ident, value)| {
        quote!(
            pub const #ident: #ident = #value;
        )
    });

    let contents = quote! {
        use crate::unary::{types::*, S, Z};

        #(#constants)*
    };

    contents.into()
}

/// **Internal implementation detail:** This proc macro generates trait implementations converting
/// type-level constants into unary representation. It will generate up to the maximum number
/// specified as the argument.
#[proc_macro]
pub fn generate_to_unary_impls(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let impls = (0..=arity_limit).scan(quote!(Z), |state, i| {
        let tokens = quote! {
            impl ToUnary for Number<#i> {
                type AsUnary = #state;
            }
        };

        *state = quote!(S<#state>);

        Some(tokens)
    });

    quote!(#(#impls)*).into()
}

/// **Internal implementation detail:** This proc macro generates trait implementations converting
/// types acting as wrappers for integer constant generics into `Choice<N>` types.
#[proc_macro]
pub fn generate_to_choice_impls(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let impls = (0..=arity_limit).scan(quote!(Z), |state, i| {
        let tokens = quote! {
            impl ToChoice for #state {
                type AsChoice = Choice<#i>;
            }
        };

        *state = quote!(S<#state>);

        Some(tokens)
    });

    quote!(#(#impls)*).into()
}

/// **Internal implementation detail:** This proc macro generates trait implementations converting
/// types acting as wrappers for integer constant generics into `Continue<N>` types.
#[proc_macro]
pub fn generate_to_continue_impls(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let impls = (0..=arity_limit).scan(quote!(Z), |state, i| {
        let tokens = quote! {
            impl ToContinue for #state {
                type AsContinue = Continue<#i>;
            }
        };

        *state = quote!(S<#state>);

        Some(tokens)
    });

    quote!(#(#impls)*).into()
}
