use {
    proc_macro2::Span,
    quote::ToTokens,
    syn::{
        braced, parenthesized,
        parse::{Error, Parse, ParseStream, Result},
        token, Ident, Lifetime, Token, Type,
    },
};

use crate::{Ast, AstDef, Invocation, Modifier};

mod kw {
    syn::custom_keyword!(recv);
    syn::custom_keyword!(send);
    syn::custom_keyword!(call);
    syn::custom_keyword!(choose);
    syn::custom_keyword!(offer);
}

struct ChoiceArm {
    index: u8,
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
            .and_then(|s| s.parse::<u8>().map_err(|e| input.error(e)))?;
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
            // Ast::Recv: recv(<type>)
            input.parse::<kw::recv>()?;
            let content;
            parenthesized!(content in input);
            let ty = content.parse::<Type>()?;
            Ok(Ast::Recv(ty.into_token_stream().to_string()))
        } else if lookahead.peek(kw::send) {
            // Ast::Send: send(<type>)
            input.parse::<kw::send>()?;
            let content;
            parenthesized!(content in input);
            let ty = content.parse::<Type>()?;
            Ok(Ast::Send(ty.into_token_stream().to_string()))
        } else if lookahead.peek(kw::call) {
            // Ast::Call: call(<type>)
            input.parse::<kw::call>()?;
            let content;
            parenthesized!(content in input);
            let ty = content.parse::<Type>()?;
            Ok(Ast::Call(ty.into_token_stream().to_string()))
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
                        format!("expected index {}, found {}", i, choice_arm.index),
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
                        format!("expected index {}, found {}", i, choice_arm.index),
                    ));
                }

                arm_asts.push(choice_arm.arm);
            }

            Ok(Ast::Offer(arm_asts))
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
            Ok(Ast::Type(ty.to_token_stream().to_string()))
        }
    }
}

impl Parse for AstDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let modifier = if input.peek(Token![priv]) {
            input.parse::<Token![priv]>()?;
            Some(Modifier::Priv)
        } else if input.peek(Token![pub]) {
            input.parse::<Token![pub]>()?;
            Some(Modifier::Pub)
        } else {
            None
        };

        input.parse::<Token![type]>()?;
        let lhs = input.parse::<Type>()?;
        input.parse::<Token![=]>()?;
        let rhs = input.parse::<Ast>()?;
        input.parse::<Token![;]>()?;

        Ok(AstDef {
            modifier,
            lhs: lhs.to_token_stream().to_string(),
            rhs,
        })
    }
}

impl Parse for Invocation {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut defs = Vec::new();
        while !input.is_empty() {
            defs.push(input.parse::<AstDef>()?);
        }

        Ok(Invocation { defs })
    }
}
