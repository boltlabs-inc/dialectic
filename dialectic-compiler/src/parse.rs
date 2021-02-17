use {
    proc_macro2::Span,
    quote::ToTokens,
    syn::{
        braced,
        parse::{Error, Parse, ParseStream, Result},
        token, Ident, Lifetime, Token, Type,
    },
};

use crate::Ast;

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
            // Ast::Recv: recv <type>
            input.parse::<kw::recv>()?;
            let ty = input.parse::<Type>()?;
            Ok(Ast::Recv(ty.into_token_stream().to_string()))
        } else if lookahead.peek(kw::send) {
            // Ast::Send: send <type>
            input.parse::<kw::send>()?;
            let ty = input.parse::<Type>()?;
            Ok(Ast::Send(ty.into_token_stream().to_string()))
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
                Ok(Ast::Call(Box::new(Ast::Type(
                    ty.into_token_stream().to_string(),
                ))))
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
                            "expected index {} in `choose` structure, found {}",
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
                            "expected index {} in `offer` structure, found {}",
                            i, choice_arm.index
                        ),
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
