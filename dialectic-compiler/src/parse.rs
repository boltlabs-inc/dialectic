//! The parser for the surface syntax of the `Session!` macro.

use {
    proc_macro2::Span,
    syn::{
        braced,
        parse::{Error, Parse, ParseStream, Result},
        spanned::Spanned as SpannedExt,
        token, Lifetime, LitInt, Token, Type,
    },
};

use crate::{
    syntax::{Invocation, Syntax},
    Spanned,
};

mod kw {
    use super::*;

    syn::custom_keyword!(recv);
    syn::custom_keyword!(send);
    syn::custom_keyword!(call);
    syn::custom_keyword!(choose);
    syn::custom_keyword!(offer);
    syn::custom_keyword!(split);

    /// Return `true` if the next token is one of our keywords.
    pub fn peek_any(input: ParseStream) -> bool {
        input.peek(recv)
            || input.peek(send)
            || input.peek(call)
            || input.peek(choose)
            || input.peek(offer)
            || input.peek(split)
    }
}

impl Parse for Invocation {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut nodes = Vec::new();
        while !input.is_empty() {
            let terminator_required = requires_terminator(&input);
            nodes.push(input.parse::<Spanned<Syntax>>()?);

            if !input.is_empty() && (terminator_required || input.peek(Token![;])) {
                input.parse::<Token![;]>()?;
            }
        }

        let ast = Spanned {
            inner: Syntax::Block(nodes),
            span: Span::call_site(),
        };
        Ok(Invocation { syntax: ast })
    }
}

/// The direction of a split arm, either `->` or `<-`.
#[derive(Clone, Copy, PartialEq)]
enum Direction {
    /// The transmit direction `->`.
    Transmit,
    /// The receive direction `<-`.
    Receive,
}

/// A split arm, consisting of a [`Direction`] and a ([`Spanned`]) [`Syntax`] for the body of the
/// arm: `<- ...` or `-> ...`. This parser also parses a terminator, if necessary or present.
struct SplitArm {
    /// Which direction the split arm operates in.
    dir: Direction,
    /// The body of the arm.
    body: Spanned<Syntax>,
}

impl Parse for SplitArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        let dir = if lookahead.peek(Token![->]) {
            let _ = input.parse::<Token![->]>()?;
            Direction::Transmit
        } else if lookahead.peek(Token![<-]) {
            let _ = input.parse::<Token![<-]>()?;
            Direction::Receive
        } else {
            return Err(lookahead.error());
        };
        let terminator_required = requires_terminator(&input);
        let arm = input.parse::<Spanned<Syntax>>()?;
        if !input.is_empty() && (terminator_required || input.peek(Token![,])) {
            input.parse::<Token![,]>()?;
        }
        Ok(SplitArm { dir, body: arm })
    }
}

/// An arm of a choice (either in `offer` or `choose`), consisting of an index and the code it
/// refers to: `N => ...`. This parser also parses a terminator, if necessary or if it is simply
/// present.
struct ChoiceArm {
    /// The index of the arm, i.e. `0`, `1`, ... `127`.
    index: Spanned<usize>,
    /// The body of the arm.
    body: Spanned<Syntax>,
}

impl Parse for ChoiceArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let index_lit = input.parse::<LitInt>()?;
        let index_span = index_lit.span();
        let index = index_lit
            .base10_parse::<usize>()
            .map_err(|e| input.error(e))?;
        let _ = input.parse::<Token![=>]>()?;
        let terminator_required = requires_terminator(&input);
        let arm = input.parse::<Spanned<Syntax>>()?;
        if !input.is_empty() && (terminator_required || input.peek(Token![,])) {
            input.parse::<Token![,]>()?;
        }
        Ok(ChoiceArm {
            index: Spanned {
                inner: index,
                span: index_span,
            },
            body: arm,
        })
    }
}

/// A block of statements: `{ ... }`.
struct Block(Spanned<Syntax>);

/// Returns `true` if the next token in the `ParseStream` starts an expression that would require a
/// terminator afterwards.
fn requires_terminator(input: ParseStream) -> bool {
    // It's easier to check if a terminator isn't required because checking to see whether a
    // terminator *is* required means checking to see if we're looking at a directly injected type,
    // which means checking to see whether it fits Rust type syntax. This is highly
    // complex/downright ridiculous to do using just syn's peek functionality.
    let terminator_not_required = input.peek(token::Brace)
        || (input.peek(kw::call) && input.peek2(token::Brace))
        || input.peek(kw::split)
        || input.peek(kw::choose)
        || input.peek(kw::offer)
        || input.peek(Token![loop])
        || input.peek(Lifetime);
    !terminator_not_required
}

impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Self> {
        // Ast::Block: { <Ast>; <Ast>; ... }
        let block_span = input.span();
        let content;
        braced!(content in input);
        let mut nodes = Vec::new();
        while !content.is_empty() {
            let terminator_required = requires_terminator(&content);
            nodes.push(content.parse::<Spanned<Syntax>>()?);

            if !content.is_empty() && (terminator_required || content.peek(Token![;])) {
                content.parse::<Token![;]>()?;
            }
        }

        Ok(Block(Spanned {
            inner: Syntax::Block(nodes),
            span: block_span,
        }))
    }
}

impl Parse for Spanned<Syntax> {
    fn parse(input: ParseStream) -> Result<Self> {
        // Take a parsed, spanned piece of syntax, join its span with the mutably referenced span,
        // and then return the unmodified thing; helper for joining spans of multiple bits of syntax
        // together while parsing without lots of if-lets.
        fn with_span<T: SpannedExt>(span: &mut Span, thing: T) -> T {
            *span = span.join(thing.span()).unwrap_or(*span);
            thing
        }

        let lookahead = input.lookahead1();
        if lookahead.peek(kw::recv) {
            // Ast::Recv: recv <type>
            let recv_span = input.parse::<kw::recv>()?.span();
            Ok(Spanned {
                inner: Syntax::Recv(input.parse::<Type>()?),
                span: recv_span,
            })
        } else if lookahead.peek(kw::send) {
            // Ast::Send: send <type>
            let send_span = input.parse::<kw::send>()?.span();
            Ok(Spanned {
                inner: Syntax::Send(input.parse::<Type>()?),
                span: send_span,
            })
        } else if lookahead.peek(kw::call) {
            // Ast::Call: call <type> or call <block>
            let call_span = input.parse::<kw::call>()?.span();
            let lookahead = input.lookahead1();

            let callee = if lookahead.peek(token::Brace) {
                input.parse::<Block>()?.0
            } else if kw::peek_any(input) {
                return Err(input.error("expected a block or a type, but found a keyword"));
            } else {
                let ty = input.parse::<Type>().map_err(|mut e| {
                    e.combine(lookahead.error());
                    e
                })?;

                let span = ty.span();

                Spanned {
                    inner: Syntax::Type(ty),
                    span,
                }
            };

            Ok(Spanned {
                inner: Syntax::Call(Box::new(callee)),
                span: call_span,
            })
        } else if lookahead.peek(kw::choose) {
            // Ast::Choose: choose { 0 => <Ast>, 1 => <Ast>, ... }
            let kw_span = input.parse::<kw::choose>()?.span();
            let choose_span = kw_span.join(input.span()).unwrap_or(kw_span);

            let carrier_type = if input.peek(token::Brace) {
                None
            } else {
                Some(input.parse()?)
            };

            let content;
            braced!(content in input);
            let mut choice_arms = Vec::new();
            while !content.is_empty() {
                choice_arms.push(content.parse::<ChoiceArm>()?);
            }

            let mut arm_asts = Vec::new();
            for (i, choice_arm) in choice_arms.into_iter().enumerate() {
                if i != choice_arm.index.inner as usize {
                    return Err(Error::new(
                        choice_arm.index.span,
                        format!(
                            "expected index {} in `choose` construct, found {}",
                            i, choice_arm.index.inner
                        ),
                    ));
                }

                arm_asts.push(choice_arm.body);
            }

            Ok(Spanned {
                inner: Syntax::Choose(arm_asts, carrier_type),
                span: choose_span,
            })
        } else if lookahead.peek(kw::offer) {
            // Ast::Offer: offer { 0 => <Ast>, 1 => <Ast>, ... }
            let kw_span = input.parse::<kw::offer>()?.span();
            let offer_span = kw_span.join(input.span()).unwrap_or(kw_span);

            let carrier_type = if input.peek(token::Brace) {
                None
            } else {
                Some(input.parse()?)
            };

            let content;
            braced!(content in input);
            let mut choice_arms = Vec::new();
            while !content.is_empty() {
                choice_arms.push(content.parse::<ChoiceArm>()?);
            }

            let mut arm_asts = Vec::new();
            for (i, choice_arm) in choice_arms.into_iter().enumerate() {
                if i != choice_arm.index.inner as usize {
                    return Err(Error::new(
                        choice_arm.index.span,
                        format!(
                            "expected index {} in `offer` construct, found {}",
                            i, choice_arm.index.inner
                        ),
                    ));
                }

                arm_asts.push(choice_arm.body);
            }

            Ok(Spanned {
                inner: Syntax::Offer(arm_asts, carrier_type),
                span: offer_span,
            })
        } else if lookahead.peek(kw::split) {
            // Ast::Split: split { <- ..., -> ... } or split { -> ..., <- ... }
            let kw_span = input.parse::<kw::split>()?.span();
            let split_span = kw_span.join(input.span()).unwrap_or(kw_span);

            let content;
            braced!(content in input);
            let mut split_arms = Vec::new();
            while !content.is_empty() {
                split_arms.push(content.parse::<SplitArm>()?);
            }

            if split_arms.len() != 2 || split_arms[0].dir == split_arms[1].dir {
                return Err(input.error(
                    "split constructs must have exactly two arms, one Transmit and one inbound",
                ));
            }

            if split_arms[0].dir == Direction::Receive {
                split_arms.swap(0, 1);
            }

            Ok(Spanned {
                inner: Syntax::Split {
                    tx_only: Box::new(split_arms[0].body.clone()),
                    rx_only: Box::new(split_arms[1].body.clone()),
                },
                span: split_span,
            })
        } else if lookahead.peek(Token![loop]) || lookahead.peek(Lifetime) {
            // Ast::Loop: 'label: loop { ... }
            let mut loop_span;
            let label = if input.peek(Lifetime) {
                loop_span = input.span();
                let lifetime = with_span(&mut loop_span, input.parse::<Lifetime>()?);
                let name = lifetime.ident.to_string();
                let _ = with_span(&mut loop_span, input.parse::<Token![:]>()?);
                let _ = with_span(&mut loop_span, input.parse::<Token![loop]>()?);
                Some(name)
            } else {
                loop_span = input.parse::<Token![loop]>()?.span();
                None
            };

            let Block(block) = input.parse::<Block>()?;
            Ok(Spanned {
                inner: Syntax::Loop(label, Box::new(block)),
                span: loop_span,
            })
        } else if lookahead.peek(Token![break]) {
            // Ast::Break: break 'label
            let mut break_span = input.parse::<Token![break]>()?.span();
            let label = if input.peek(Lifetime) {
                let lifetime = input.parse::<Lifetime>()?;
                let _ = with_span(&mut break_span, &lifetime);
                Some(lifetime.ident.to_string())
            } else {
                None
            };

            Ok(Spanned {
                inner: Syntax::Break(label),
                span: break_span,
            })
        } else if lookahead.peek(Token![continue]) {
            // Ast::Continue: continue 'label
            let mut continue_span = input.parse::<Token![continue]>()?.span();
            let label = if input.peek(Lifetime) {
                let lifetime = input.parse::<Lifetime>()?;
                let _ = with_span(&mut continue_span, &lifetime);
                Some(lifetime.ident.to_string())
            } else {
                None
            };

            Ok(Spanned {
                inner: Syntax::Continue(label),
                span: continue_span,
            })
        } else if lookahead.peek(token::Brace) {
            // Ast::Block: { <Ast>; <Ast>; ... }
            Ok(input.parse::<Block>()?.0)
        } else {
            // Attempt to parse as a direct type Ast::Type: <type> Otherwise, fail and report all
            // other errors from the lookahead.
            let ty = match input.parse::<Type>() {
                Ok(ty) => ty,
                Err(e) => {
                    let mut combined = lookahead.error();
                    combined.combine(input.error("parsing as a type failed"));
                    combined.combine(e);
                    return Err(combined);
                }
            };

            let span = ty.span();

            Ok(Spanned {
                inner: Syntax::Type(ty),
                span,
            })
        }
    }
}
