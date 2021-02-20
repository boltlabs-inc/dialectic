use {
    proc_macro2::Span,
    syn::{
        braced,
        parse::{Error, Parse, ParseStream, Result},
        spanned::Spanned,
        token, Ident, Lifetime, Token, Type,
    },
};

use crate::{Invocation, Syntax, SyntaxNode};

trait JoinSpansExt {
    fn combine<T: Spanned>(&mut self, new: T) -> T;
}

impl JoinSpansExt for Span {
    fn combine<T: Spanned>(&mut self, new: T) -> T {
        *self = self.join(new.span()).unwrap_or(*self);
        new
    }
}

mod kw {
    syn::custom_keyword!(recv);
    syn::custom_keyword!(send);
    syn::custom_keyword!(call);
    syn::custom_keyword!(choose);
    syn::custom_keyword!(offer);
    syn::custom_keyword!(split);
}

#[derive(Clone, Copy, PartialEq)]
enum Direction {
    Outbound,
    Inbound,
}

struct SplitArm {
    dir: Direction,
    arm: SyntaxNode,
}

impl Parse for SplitArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        let dir = if lookahead.peek(Token![->]) {
            input.parse::<Token![->]>()?;
            Direction::Outbound
        } else if lookahead.peek(Token![<-]) {
            input.parse::<Token![<-]>()?;
            Direction::Inbound
        } else {
            return Err(lookahead.error());
        };
        let arm = input.parse::<SyntaxNode>()?;
        Ok(SplitArm { dir, arm })
    }
}

struct ChoiceArm {
    index: usize,
    arm: SyntaxNode,
    span: Span,
}

impl Parse for ChoiceArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let index_ident = input.parse::<Ident>()?;
        let mut span = index_ident.span();
        let index = index_ident
            .to_string()
            .strip_prefix("_")
            .ok_or_else(|| input.error("expected index identifier starting with an underscore"))
            .and_then(|s| s.parse::<usize>().map_err(|e| input.error(e)))?;
        input.parse::<Token![=>]>()?;
        span = span.join(input.span()).unwrap_or(span);
        let arm = input.parse::<SyntaxNode>()?;
        Ok(ChoiceArm { index, arm, span })
    }
}

struct Block(SyntaxNode);

impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Self> {
        // Ast::Block: { <Ast>; <Ast>; ... }
        let block_span = input.span();
        let content;
        braced!(content in input);
        let nodes = content
            .parse_terminated::<SyntaxNode, Token![;]>(SyntaxNode::parse)?
            .into_iter()
            .collect();

        Ok(Block(SyntaxNode {
            expr: Syntax::Block(nodes),
            span: block_span,
        }))
    }
}

impl Parse for SyntaxNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::recv) {
            // Ast::Recv: recv <type>
            let recv_span = input.parse::<kw::recv>()?.span();
            Ok(SyntaxNode {
                expr: Syntax::Recv(input.parse::<Type>()?),
                span: recv_span,
            })
        } else if lookahead.peek(kw::send) {
            // Ast::Send: send <type>
            let send_span = input.parse::<kw::send>()?.span();
            Ok(SyntaxNode {
                expr: Syntax::Send(input.parse::<Type>()?),
                span: send_span,
            })
        } else if lookahead.peek(kw::call) {
            // Ast::Call: call <type> or call <block>
            let call_span = input.parse::<kw::call>()?.span();
            let lookahead = input.lookahead1();

            let callee = if lookahead.peek(token::Brace) {
                input.parse::<Block>()?.0
            } else {
                let ty = input.parse::<Type>().map_err(|mut e| {
                    e.combine(lookahead.error());
                    e
                })?;

                SyntaxNode {
                    span: ty.span(),
                    expr: Syntax::Type(ty),
                }
            };

            Ok(SyntaxNode {
                expr: Syntax::Call(Box::new(callee)),
                span: call_span,
            })
        } else if lookahead.peek(kw::choose) {
            // Ast::Choose: choose { _0 => <Ast>, _1 => <Ast>, ... }
            let kw_span = input.parse::<kw::choose>()?.span();
            let choose_span = kw_span.join(input.span()).unwrap_or(kw_span);

            let content;
            braced!(content in input);
            let choice_arms = content.parse_terminated::<ChoiceArm, Token![,]>(ChoiceArm::parse)?;

            let mut arm_asts = Vec::new();
            for (i, choice_arm) in choice_arms.into_iter().enumerate() {
                if i != choice_arm.index as usize {
                    return Err(Error::new(
                        choice_arm.span,
                        format!(
                            "expected index {} in `choose` construct, found {}",
                            i, choice_arm.index
                        ),
                    ));
                }

                arm_asts.push(choice_arm.arm);
            }

            Ok(SyntaxNode {
                expr: Syntax::Choose(arm_asts),
                span: choose_span,
            })
        } else if lookahead.peek(kw::offer) {
            // Ast::Offer: offer { _0 => <Ast>, _1 => <Ast>, ... }
            let kw_span = input.parse::<kw::offer>()?.span();
            let offer_span = kw_span.join(input.span()).unwrap_or(kw_span);

            let content;
            braced!(content in input);
            let choice_arms = content.parse_terminated::<ChoiceArm, Token![,]>(ChoiceArm::parse)?;

            let mut arm_asts = Vec::new();
            for (i, choice_arm) in choice_arms.into_iter().enumerate() {
                if i != choice_arm.index as usize {
                    return Err(Error::new(
                        choice_arm.span,
                        format!(
                            "expected index {} in `offer` construct, found {}",
                            i, choice_arm.index
                        ),
                    ));
                }

                arm_asts.push(choice_arm.arm);
            }

            Ok(SyntaxNode {
                expr: Syntax::Offer(arm_asts),
                span: offer_span,
            })
        } else if lookahead.peek(kw::split) {
            let kw_span = input.parse::<kw::split>()?.span();
            let split_span = kw_span.join(input.span()).unwrap_or(kw_span);

            let content;
            braced!(content in input);
            let mut split_arms = content
                .parse_terminated::<SplitArm, Token![,]>(SplitArm::parse)?
                .into_iter()
                .collect::<Vec<_>>();

            if split_arms.len() != 2 || split_arms[0].dir == split_arms[1].dir {
                return Err(input.error(
                    "split constructs must have exactly two arms, one outbound and one inbound",
                ));
            }

            if split_arms[0].dir == Direction::Inbound {
                split_arms.swap(0, 1);
            }

            Ok(SyntaxNode {
                expr: Syntax::Split(
                    Box::new(split_arms[0].arm.clone()),
                    Box::new(split_arms[1].arm.clone()),
                ),
                span: split_span,
            })
        } else if lookahead.peek(Token![loop]) || lookahead.peek(Lifetime) {
            // Ast::Loop: 'label loop { ... }
            let mut loop_span;
            let label = if input.peek(Lifetime) {
                loop_span = input.span();
                let lifetime = loop_span.combine(input.parse::<Lifetime>()?);
                let name = lifetime.ident.to_string();
                loop_span.combine(input.parse::<Token![:]>()?);
                loop_span.combine(input.parse::<Token![loop]>()?);
                Some(name)
            } else {
                loop_span = input.parse::<Token![loop]>()?.span();
                None
            };

            let Block(block) = input.parse::<Block>()?;
            Ok(SyntaxNode {
                expr: Syntax::Loop(label, Box::new(block)),
                span: loop_span,
            })
        } else if lookahead.peek(Token![break]) {
            // Ast::Break: break 'label
            let mut break_span = input.parse::<Token![break]>()?.span();
            let label = if input.peek(Lifetime) {
                let lifetime = input.parse::<Lifetime>()?;
                break_span.combine(&lifetime);
                Some(lifetime.ident.to_string())
            } else {
                None
            };

            Ok(SyntaxNode {
                expr: Syntax::Break(label),
                span: break_span,
            })
        } else if lookahead.peek(Token![continue]) {
            // Ast::Continue: continue 'label
            let mut continue_span = input.parse::<Token![continue]>()?.span();
            let label = if input.peek(Lifetime) {
                let lifetime = input.parse::<Lifetime>()?;
                continue_span.combine(&lifetime);
                Some(lifetime.ident.to_string())
            } else {
                None
            };

            Ok(SyntaxNode {
                expr: Syntax::Continue(label),
                span: continue_span,
            })
        } else if lookahead.peek(token::Brace) {
            // Ast::Block: { <Ast>; <Ast>; ... }
            Ok(input.parse::<Block>()?.0)
        } else {
            // Attempt to parse as a direct type Ast::Type: <type>
            // Otherwise, fail and report all other errors from the lookahead.
            let ty = match input.parse::<Type>() {
                Ok(ty) => ty,
                Err(e) => {
                    let mut combined = lookahead.error();
                    combined.combine(input.error("parsing as a type failed"));
                    combined.combine(e);
                    return Err(combined);
                }
            };

            Ok(SyntaxNode {
                span: ty.span(),
                expr: Syntax::Type(ty),
            })
        }
    }
}

impl Parse for Invocation {
    fn parse(input: ParseStream) -> Result<Self> {
        let nodes = input
            .parse_terminated::<SyntaxNode, Token![;]>(SyntaxNode::parse)?
            .into_iter()
            .collect::<Vec<_>>();
        let ast = SyntaxNode {
            expr: Syntax::Block(nodes),
            span: Span::call_site(),
        };
        Ok(Invocation { syntax: ast })
    }
}
