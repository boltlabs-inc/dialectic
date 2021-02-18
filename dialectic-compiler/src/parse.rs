use {
    proc_macro2::Span,
    syn::{
        braced,
        parse::{Error, Parse, ParseStream, Result},
        token, Ident, Lifetime, Token, Type,
    },
};

use crate::{Ast, Invocation};

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
    arm: Ast,
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
        let arm = input.parse::<Ast>()?;
        Ok(SplitArm { dir, arm })
    }
}

struct ChoiceArm {
    index: usize,
    arm: Ast,
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
        let arm = input.parse::<Ast>()?;
        Ok(ChoiceArm { index, arm, span })
    }
}

impl Parse for Ast {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::recv) {
            // Ast::Recv: recv <type>
            input.parse::<kw::recv>()?;
            Ok(Ast::Recv(input.parse::<Type>()?))
        } else if lookahead.peek(kw::send) {
            // Ast::Send: send <type>
            input.parse::<kw::send>()?;
            Ok(Ast::Send(input.parse::<Type>()?))
        } else if lookahead.peek(kw::call) {
            // Ast::Call: call <type> or call <block>
            input.parse::<kw::call>()?;
            let lookahead = input.lookahead1();

            if lookahead.peek(token::Brace) {
                let content;
                braced!(content in input);
                let nodes = content
                    .parse_terminated::<Ast, Token![;]>(Ast::parse)?
                    .into_iter()
                    .collect();
                Ok(Ast::Call(Box::new(Ast::Block(nodes))))
            } else {
                let ty = input.parse::<Type>().map_err(|mut e| {
                    e.combine(lookahead.error());
                    e
                })?;
                Ok(Ast::call(Ast::Type(ty)))
            }
        } else if lookahead.peek(kw::choose) {
            // Ast::Choose: choose { _0 => <Ast>, _1 => <Ast>, ... }
            input.parse::<kw::choose>()?;
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

            Ok(Ast::Choose(arm_asts))
        } else if lookahead.peek(kw::offer) {
            // Ast::Offer: offer { _0 => <Ast>, _1 => <Ast>, ... }
            input.parse::<kw::offer>()?;
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

            Ok(Ast::Offer(arm_asts))
        } else if lookahead.peek(kw::split) {
            input.parse::<kw::split>()?;
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

            Ok(Ast::Split(
                Box::new(split_arms[0].arm.clone()),
                Box::new(split_arms[1].arm.clone()),
            ))
        } else if lookahead.peek(Token![loop]) || lookahead.peek(Lifetime) {
            // Ast::Loop: 'label loop { ... }
            let label = if input.peek(Lifetime) {
                let name = input.parse::<Lifetime>()?.ident.to_string();
                input.parse::<Token![:]>()?;
                Some(name)
            } else {
                None
            };

            input.parse::<Token![loop]>()?;
            let content;
            braced!(content in input);
            let nodes = content
                .parse_terminated::<Ast, Token![;]>(Ast::parse)?
                .into_iter()
                .collect();
            Ok(Ast::Loop(label, Box::new(Ast::Block(nodes))))
        } else if lookahead.peek(Token![break]) {
            // Ast::Break: break 'label
            input.parse::<Token![break]>()?;
            let label = if input.peek(Lifetime) {
                Some(input.parse::<Lifetime>()?.ident.to_string())
            } else {
                None
            };
            Ok(Ast::Break(label))
        } else if lookahead.peek(Token![continue]) {
            // Ast::Continue: continue 'label
            input.parse::<Token![continue]>()?;
            let label = if input.peek(Lifetime) {
                Some(input.parse::<Lifetime>()?.ident.to_string())
            } else {
                None
            };
            Ok(Ast::Continue(label))
        } else if lookahead.peek(token::Brace) {
            // Ast::Block: { <Ast>; <Ast>; ... }
            let content;
            braced!(content in input);
            let nodes = content
                .parse_terminated::<Ast, Token![;]>(Ast::parse)?
                .into_iter()
                .collect();
            Ok(Ast::Block(nodes))
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
            Ok(Ast::Type(ty))
        }
    }
}

impl Parse for Invocation {
    fn parse(input: ParseStream) -> Result<Self> {
        let nodes = input
            .parse_terminated::<Ast, Token![;]>(Ast::parse)?
            .into_iter()
            .collect::<Vec<_>>();
        let ast = Ast::Block(nodes);
        Ok(Invocation { ast })
    }
}
