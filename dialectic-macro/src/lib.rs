#![forbid(broken_intra_doc_links)]

extern crate proc_macro;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    braced, parse::Parse, parse::ParseStream, parse_macro_input, punctuated::Punctuated,
    spanned::Spanned, Arm, Ident, LitInt, Pat, Token,
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
    Session! { offer { 0 => {}, 1 => {} } },
    Offer<(Done, Done)>
);

type_eq!(
    Session! { choose { 0 => {}, 1 => {} } },
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
            0 => {
                send i64;
                call { continue };
                recv i64;
            },
            1 => break,
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
            0 => Twice<Parity>,
            1 => break,
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
///         0 => send i64,
///         1 => recv String,
///     }
/// };
///
/// let (c1, c2) = GiveOrTake::channel(|| mpsc::channel(1));
///
/// // Spawn a thread to offer a choice
/// let t1 = tokio::spawn(async move {
///     offer!(in c2 {
///         0 => { c2.recv().await?; },
///         1 => { c2.send("Hello!".to_string()).await?; },
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
        let _ = input.parse::<Token![in]>()?;
        let chan = input.parse::<Ident>()?;
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
            if choice > 256 {
                let message = format!("at most 256 arms (labeled `0` through `255` in ascending order) are permitted in the `offer!` macro; this arm is number {}", choice);
                errors.push(syn::Error::new(arm.span(), message));
            }
            match &arm.pat {
                Pat::Lit(pat_lit) => {
                    let lit_int = match &*pat_lit.expr {
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Int(lit_int),
                            ..
                        }) => lit_int,
                        _ => {
                            let message = "expected a usize literal";
                            errors.push(syn::Error::new(pat_lit.expr.span(), message));
                            continue;
                        }
                    };

                    match lit_int.base10_parse::<usize>() {
                        Ok(n) if n == choice => {}
                        Ok(_) => {
                            let message = format!("expected the `usize` literal `{}` for this arm of the `offer!` macro (note: arms must be in ascending order)", choice);
                            errors.push(syn::Error::new(pat_lit.span(), message));
                        }
                        Err(e) => {
                            let message = format!("could not parse literal: `{}`", e);
                            errors.push(syn::Error::new(pat_lit.span(), message));
                        }
                    }
                }
                _ => {
                    let message = format!("expected the `usize` literal `{}` for this arm (note: arms must be in ascending order)", choice);
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

enum Mutability {
    Val(Option<Token![move]>),
    Ref(Token![ref]),
    Mut(Token![ref], Token![mut]),
}

impl ToTokens for Mutability {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        use Mutability::*;
        let dialectic_path = dialectic_compiler::dialectic_path();
        stream.extend(match self {
            Val(t) => quote_spanned!(t.span()=> #dialectic_path::backend::Val),
            Ref(t) => quote_spanned!(t.span()=> #dialectic_path::backend::Ref),
            Mut(t1, t2) => quote_spanned!(t1.span().join(t2.span()).unwrap_or(t2.span())=> #dialectic_path::backend::Mut),
        })
    }
}

impl Parse for Mutability {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let lookahead = input.lookahead1();
        let mutability = if lookahead.peek(Token![ref]) {
            let ref_token = input.parse()?;
            if input.peek(Token![mut]) {
                Mutability::Mut(ref_token, input.parse()?)
            } else {
                Mutability::Ref(ref_token)
            }
        } else {
            Mutability::Val(input.parse()?)
        };

        Ok(mutability)
    }
}

struct TransmitterSpec {
    name: syn::Type,
    types: Punctuated<(Mutability, syn::Type), Token![,]>,
}

struct ReceiverSpec {
    name: syn::Type,
    types: Punctuated<syn::Type, Token![,]>,
}

impl Parse for TransmitterSpec {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        fn parse_convention_type_pair(input: ParseStream) -> Result<(Mutability, syn::Type), syn::Error> {
            Ok((input.parse()?, input.parse()?))
        }

        let name: syn::Type = input.parse()?;
        if input.is_empty() {
            Ok(TransmitterSpec {
                name,
                types: Punctuated::new(),
            })
        } else {
            let _for: Token![for] = input.parse()?;
            let types = if input.is_empty() {
                Punctuated::new()
            } else {
                input.parse_terminated(parse_convention_type_pair)?
            };
            Ok(TransmitterSpec {
                name,
                types,
            })
        }
    }
}

impl Parse for ReceiverSpec {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let name: syn::Type = input.parse()?;
        if input.is_empty() {
            Ok(ReceiverSpec {
                name,
                types: Punctuated::new(),
            })
        } else {
            let _for: Token![for] = input.parse()?;
            let types = if input.is_empty() {
                Punctuated::new()
            } else {
                input.parse_terminated(syn::Type::parse)?
            };
            Ok(ReceiverSpec { name, types })
        }
    }
}

/// Get a mutable reference to the `where` predicates of an item, if the item supports `where`
/// predicates. Creates empty `where` clause if none exists yet.
fn where_predicates_mut(
    item: &mut syn::Item,
) -> Option<&mut Punctuated<syn::WherePredicate, Token![,]>> {
    use syn::Item::*;
    let span = item.span();
    let maybe_where = match item {
        Fn(i) => &mut i.sig.generics.where_clause,
        Enum(i) => &mut i.generics.where_clause,
        Impl(i) => &mut i.generics.where_clause,
        Struct(i) => &mut i.generics.where_clause,
        Trait(i) => &mut i.generics.where_clause,
        TraitAlias(i) => &mut i.generics.where_clause,
        Type(i) => &mut i.generics.where_clause,
        Union(i) => &mut i.generics.where_clause,
        _ => return None,
    };
    if let Some(where_clause) = maybe_where {
        Some(&mut where_clause.predicates)
    } else {
        let clause = syn::WhereClause {
            where_token: syn::token::Where { span },
            predicates: Punctuated::new(),
        };
        *maybe_where = Some(clause);
        Some(&mut maybe_where.as_mut().unwrap().predicates)
    }
}

/// In situations where the transmitting backend for a channel is generic, explicitly writing down
/// all the trait bounds necessary to implement a protocol for that parameterized backend can be a
/// lot of boilerplate. The `Transmitter` attribute macro abbreviates these bounds by modifying the
/// `where` clause of the item to which it is attached.
///
/// This macro may be attached to any item capable of supporting a `where`-clause: an `enum`
/// definition, `fn` item (top level or in a trait definition or implementation), `impl` block,
/// `struct` definition, `trait` definition, `type` synonym, or `union` definition.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
///
/// #[Transmitter(Tx move for bool, i64)]
/// async fn foo<Tx, Rx>(
///     chan: Chan<Session!{ send bool; send i64 }, Tx, Rx>
/// ) -> Result<(), Tx::Error>
/// where
///     Rx: Send + 'static,
/// {
///     let chan = chan.send(true).await?;
///     let chan = chan.send(42).await?;
///     chan.close();
///     Ok(())
/// }
/// ```
///
/// # Syntax
///
/// The `Transmitter` attribute takes the name of the transmitter for which the bounds will be
/// generated, an optional *calling convention* (one of `move`, `ref`, or `mut`), and, optionally,
/// the keyword `for` followed by a comma-separated list of types.
///
/// Some example invocations:
///
/// ```
/// # use dialectic::prelude::*;
/// #[Transmitter(Tx)]
/// # fn a<Tx>() {}
/// #[Transmitter(Tx)]
/// # fn b<Tx>() {}
/// #[Transmitter(Tx for bool)]
/// # fn c<Tx>() {}
/// #[Transmitter(Tx for ref bool)]
/// # fn d<Tx>() {}
/// #[Transmitter(Tx for ref mut bool)]
/// # fn e<Tx>() {}
/// #[Transmitter(Tx for bool, i64, Vec<String>)]
/// # fn f<Tx>() {}
/// #[Transmitter(Tx for bool, ref i64, ref mut Vec<String>)]
/// ```
///
/// # Expansion
///
/// The attribute adds extra bounds to the `where`-clause of the item to which it is attached (if
/// the item does not have a `where-clause, one will be created containing the bounds).
///
/// For a transmitter type `Tx`, optional calling conventions `CN?` (none, or one of `move`, `ref`,
/// or `mut`), and types `T1`, `T2`, `...`, the invocation:
///
/// ```ignore
/// #[Transmitter(Tx for C1? T1, C2? T2, ...)]
/// fn f<Tx>() {}
/// ```
///
/// ...translates to the following bounds:
///
/// ```
/// use dialectic::prelude::*;
///
/// # type C1 = Ref;
/// # type C2 = Ref;
/// # struct T1;
/// # struct T2;
/// #
/// fn f<Tx>()
/// where
///     Tx: Transmitter + Send + 'static,
///     // For each of the types `T1`, `T2`, ...
///     // If the convention is unspecified, `C` is left unspecified;
///     // otherwise, we translate into `call_by` conventions using
///     // `move` => `Val`, `ref` => `Ref`, and `mut` => `Mut`
///     Tx: Transmit<T1, C1>,
///     Tx: Transmit<T2, C2>,
///     // ...
/// {}
/// ```
#[allow(non_snake_case)]
#[proc_macro_attribute]
pub fn Transmitter(
    params: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let TransmitterSpec {
        name,
        types,
    } = parse_macro_input!(params as TransmitterSpec);
    let dialectic_path = dialectic_compiler::dialectic_path();
    let mut item = parse_macro_input!(input as syn::Item);
    if let Some(predicates) = where_predicates_mut(&mut item) {
        predicates.push(
            syn::parse_quote! {
                #name: ::std::marker::Send
                    + #dialectic_path::backend::Transmitter
                    + 'static
            }
        );
        for (mutability, ty) in types {
            predicates.push(syn::parse_quote! {
                #name: #dialectic_path::backend::Transmit<#ty, #mutability>
            });
        }
        item.into_token_stream().into()
    } else {
        let message = "unexpected kind of item for `Transmitter` attribute: expecting `enum`, `fn`, `impl`, `struct`, `trait`, `type`, or `union`";
        syn::Error::new(item.span(), message)
            .into_compile_error()
            .into_token_stream()
            .into()
    }
}

/// In situations where the transmitting backend for a channel is generic, explicitly writing down
/// all the trait bounds necessary to implement a protocol for that parameterized backend can be a
/// lot of boilerplate. The `Receiver` attribute macro abbreviates these bounds by modifying the
/// `where` clause of the item to which it is attached.
///
/// This macro may be attached to any item capable of supporting a `where`-clause: an `enum`
/// definition, `fn` item (top level or in a trait definition or implementation), `impl` block,
/// `struct` definition, `trait` definition, `type` synonym, or `union` definition.
///
/// # Examples
///
/// ```
/// use dialectic::prelude::*;
///
/// #[Receiver(Rx for bool, i64)]
/// async fn foo<Tx, Rx>(
///     chan: Chan<Session!{ recv bool; recv i64 }, Tx, Rx>
/// ) -> Result<(), Rx::Error>
/// where
///     Tx: Send + 'static,
/// {
///     let (_, chan) = chan.recv().await?;
///     let (_, chan) = chan.recv().await?;
///     chan.close();
///     Ok(())
/// }
/// ```
///
/// # Syntax
///
/// The `Receiver` attribute takes the name of the receiver for which the bounds will be generated,
/// and, optionally, the keyword `for` followed by a comma-separated list of types.
///
/// Some example invocations:
///
/// ```
/// # use dialectic::prelude::*;
/// #[Receiver(Rx)]
/// # fn a<Rx>() {}
/// #[Receiver(Rx for bool)]
/// # fn b<Rx>() {}
/// #[Receiver(Rx for bool, i64, Vec<String>)]
/// # fn c<Rx>() {}
/// ```
///
/// # Expansion
///
/// The attribute adds extra bounds to the `where`-clause of the item to which it is attached (if
/// the item does not have a `where-clause, one will be created containing the bounds).
///
/// For a transmitter type `Rx`, and types `T1`, `T2`, `...`, the invocation:
///
/// ```ignore
/// #[Receiver(Rx for T1, T2, ...)]
/// fn f<Rx>() {}
/// ```
///
/// ...translates to the following bounds:
///
/// ```
/// use dialectic::prelude::*;
///
/// # struct T1;
/// # struct T2;
/// #
/// fn f<Rx>()
/// where
///     Rx: Receiver + Send + 'static,
///     // For each of the types `T1`, `T2`, ...
///     Rx: Receive<T1>,
///     Rx: Receive<T2>,
///     // ...
/// {}
/// ```
#[allow(non_snake_case)]
#[proc_macro_attribute]
pub fn Receiver(
    params: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let dialectic_path = dialectic_compiler::dialectic_path();
    let ReceiverSpec { name, types } = parse_macro_input!(params as ReceiverSpec);
    let mut item = parse_macro_input!(input as syn::Item);
    if let Some(predicates) = where_predicates_mut(&mut item) {
        predicates.push(syn::parse_quote! {
            #name: ::std::marker::Send
            + #dialectic_path::backend::Receiver
            + 'static
        });
        for ty in types {
            predicates.push(syn::parse_quote! {
                #name: #dialectic_path::backend::Receive<#ty>
            });
        }
        item.into_token_stream().into()
    } else {
        let message = "unexpected kind of item for `Receiver` attribute: expecting `enum`, `fn`, `impl`, `struct`, `trait`, `type`, or `union`";
        syn::Error::new(item.span(), message)
            .into_compile_error()
            .into_token_stream()
            .into()
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

/// **Internal implementation detail:** This proc macro generates trait implementations of `ToUnary`
/// and `ToConstant` which convert type-level constants into unary representation, and vice versa.
/// It will generate up to the maximum number specified as the argument.
#[proc_macro]
pub fn generate_unary_conversion_impls(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arity_limit = parse_macro_input!(input as LitInt)
        .base10_parse::<usize>()
        .unwrap();

    let impls = (0..=arity_limit).scan(quote!(Z), |state, i| {
        let tokens = quote! {
            impl ToUnary for Number<#i> {
                type AsUnary = #state;
            }

            impl ToConstant for #state {
                type AsConstant = Number<#i>;
            }
        };

        *state = quote!(S<#state>);

        Some(tokens)
    });

    quote!(#(#impls)*).into()
}
