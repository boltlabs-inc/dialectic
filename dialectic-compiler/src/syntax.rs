use {
    lazy_static::lazy_static,
    proc_macro2::{Span, TokenStream},
    quote::{quote, ToTokens},
    std::fmt,
    syn::{Error, Ident, Type},
    thunderdome::Index,
};

use crate::{
    cfg::{Cfg, Ir},
    target::Target,
    CompileError, Spanned,
};

/// A shim for parsing the root level of a macro invocation, so that a session type may be written
/// as a block w/o an extra layer of braces.
#[derive(Debug, Clone)]
pub struct Invocation {
    pub syntax: Spanned<Syntax>,
}

/// The surface syntax for a macro invocation: a single statement-like item, or a block of them.
///
/// While the [`Target`] of the compiler (and its corresponding types in the Dialectic library) have
/// continuations for almost every expression, the surface syntax is not in continuation passing
/// style, instead encoding sequences of operations using the `;` operator within blocks.
#[derive(Debug, Clone)]
pub enum Syntax {
    Recv(Type),
    Send(Type),
    Call(Box<Spanned<Syntax>>),
    Choose(Vec<Spanned<Syntax>>),
    Offer(Vec<Spanned<Syntax>>),
    Split(Box<Spanned<Syntax>>, Box<Spanned<Syntax>>),
    Loop(Option<String>, Box<Spanned<Syntax>>),
    Break(Option<String>),
    Continue(Option<String>),
    Block(Vec<Spanned<Syntax>>),
    Type(Type),
}

impl Syntax {
    pub fn recv(ty: &str) -> Self {
        Syntax::Recv(syn::parse_str(ty).unwrap())
    }

    pub fn send(ty: &str) -> Self {
        Syntax::Send(syn::parse_str(ty).unwrap())
    }

    pub fn call(callee: impl Into<Spanned<Syntax>>) -> Self {
        Syntax::Call(Box::new(callee.into()))
    }

    pub fn loop_(label: Option<String>, body: impl Into<Spanned<Syntax>>) -> Self {
        Syntax::Loop(label, Box::new(body.into()))
    }

    pub fn type_(ty: &str) -> Self {
        Syntax::Type(syn::parse_str(ty).unwrap())
    }
}

impl Spanned<Syntax> {
    pub fn to_session(&self) -> Result<Target, Error> {
        let mut cfg = Cfg::new();
        let head = self.to_cfg(&mut cfg, &mut Vec::new()).0;
        cfg.eliminate_breaks(head);
        cfg.to_target(head)
    }

    fn to_cfg<'a>(&'a self, cfg: &mut Cfg, env: &mut Vec<&'a Option<String>>) -> (Index, Index) {
        use Syntax::*;
        let ir = match &self.inner {
            Recv(ty) => Ir::Recv(ty.clone()),
            Send(ty) => Ir::Send(ty.clone()),
            Type(ty) => Ir::Type(ty.clone()),
            Call(callee) => {
                let callee_node = callee.to_cfg(cfg, env).0;
                Ir::Call(callee_node)
            }
            Split(tx, rx) => {
                let tx_node = tx.to_cfg(cfg, env).0;
                let rx_node = rx.to_cfg(cfg, env).0;
                Ir::Split(tx_node, rx_node)
            }
            Choose(choices) => {
                let choice_nodes = choices
                    .iter()
                    .map(|choice| choice.to_cfg(cfg, env).0)
                    .collect();
                Ir::Choose(choice_nodes)
            }
            Offer(choices) => {
                let choice_nodes = choices
                    .iter()
                    .map(|choice| choice.to_cfg(cfg, env).0)
                    .collect();
                Ir::Offer(choice_nodes)
            }
            Loop(maybe_label, body) => {
                // Convert the body in the environment with this loop label
                env.push(maybe_label);
                let (head, tail) = body.to_cfg(cfg, env);
                env.pop();

                // Set the continuation for the loop if it doesn't end in a `Break` or `Continue`
                match cfg[tail].expr {
                    Ir::Break(_) | Ir::Continue(_) => {}
                    _ => {
                        let continue0 = cfg.singleton(Ir::Continue(0));
                        cfg[tail].next = Some(continue0);
                    }
                }

                // The `Ir` for this loop just wraps its body
                let ir = Ir::Loop(head);

                // Check to ensure the environment does not already contain this label
                if maybe_label.is_some() && env.contains(&maybe_label) {
                    Ir::Error(
                        CompileError::ShadowedLabel(maybe_label.clone().unwrap()),
                        Some(cfg.spanned(ir, self.span)),
                    )
                } else {
                    ir
                }
            }
            Continue(label) => {
                if env.is_empty() {
                    Ir::Error(CompileError::ContinueOutsideLoop, None)
                } else {
                    match label {
                        None => Ir::Continue(0),
                        Some(label) => {
                            match env.iter().rev().position(|&l| l.as_ref() == Some(label)) {
                                None => {
                                    Ir::Error(CompileError::UndeclaredLabel(label.clone()), None)
                                }
                                Some(index) => Ir::Continue(index),
                            }
                        }
                    }
                }
            }
            Break(label) => {
                if env.is_empty() {
                    Ir::Error(CompileError::BreakOutsideLoop, None)
                } else {
                    match label {
                        None => Ir::Break(0),
                        Some(label) => {
                            match env.iter().rev().position(|&l| l.as_ref() == Some(label)) {
                                None => {
                                    Ir::Error(CompileError::UndeclaredLabel(label.clone()), None)
                                }
                                Some(index) => Ir::Break(index),
                            }
                        }
                    }
                }
            }
            Block(stmts) => {
                let mut next_head = None;
                let mut next_tail = None;
                for stmt in stmts.iter().rev() {
                    let (head, tail) = stmt.to_cfg(cfg, env);
                    next_tail = next_tail.or(Some(tail));
                    cfg[tail].next = next_head;
                    next_head = Some(head);
                }

                let head = next_head.unwrap_or_else(|| cfg.spanned(Ir::Done, self.span));
                let tail = next_tail.unwrap_or(head);

                // Only case where we return a differing head and tail, since a `Block` is the only
                // expression to be compiled into multiple nodes
                return (head, tail);
            }
        };

        let node = cfg.spanned(ir, self.span);
        (node, node)
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Target::*;
        match self {
            Done => write!(f, "Done")?,
            Recv(t, s) => write!(f, "Recv<{}, {}>", t.to_token_stream(), s)?,
            Send(t, s) => write!(f, "Send<{}, {}>", t.to_token_stream(), s)?,
            Loop(s) => write!(f, "Loop<{}>", s)?,
            Split(s, p, q) => write!(f, "Split<{}, {}, {}>", s, p, q)?,
            Call(s, p) => write!(f, "Call<{}, {}>", s, p)?,
            Then(s, p) => write!(f, "<{} as Then<{}>>::Combined", s, p)?,
            Choose(cs) => {
                let count = cs.len();
                write!(f, "Choose<(")?;
                for (i, c) in cs.iter().enumerate() {
                    write!(f, "{}", c)?;
                    if i + 1 < count {
                        write!(f, ", ")?;
                    }
                }
                if count == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")>")?;
            }
            Offer(cs) => {
                let count = cs.len();
                write!(f, "Offer<(")?;
                for (i, c) in cs.iter().enumerate() {
                    write!(f, "{}", c)?;
                    if i + 1 < count {
                        write!(f, ", ")?;
                    }
                }
                if count == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")>")?;
            }
            Continue(n) => {
                write!(f, "Continue")?;
                if *n > 0 {
                    write!(f, "<_{}>", n)?;
                }
            }
            Type(s) => write!(f, "{}", s.to_token_stream())?,
        }
        Ok(())
    }
}

impl ToTokens for Target {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use Target::*;

        lazy_static! {
            static ref CRATE_NAME: String = proc_macro_crate::crate_name("dialectic")
                .unwrap_or_else(|_| "dialectic".to_owned());
        }

        let c = Ident::new(&**CRATE_NAME, Span::call_site());

        match self {
            Done => quote! { #c::types::Done }.to_tokens(tokens),
            Recv(t, s) => quote! { #c::types::Recv<#t, #s> }.to_tokens(tokens),
            Send(t, s) => quote! { #c::types::Send<#t, #s> }.to_tokens(tokens),
            Loop(s) => quote! { #c::types::Loop<#s> }.to_tokens(tokens),
            Split(s, p, q) => quote! { #c::types::Split<#s, #p, #q> }.to_tokens(tokens),
            Call(s, p) => quote! { #c::types::Call<#s, #p> }.to_tokens(tokens),
            Then(s, p) => quote! { <#s as #c::types::Then<#p>>::Combined }.to_tokens(tokens),
            Choose(cs) => quote! { #c::types::Choose<(#(#cs,)*)> }.to_tokens(tokens),
            Offer(cs) => quote! { #c::types::Offer<(#(#cs,)*)> }.to_tokens(tokens),
            Continue(n) => {
                if *n > 0 {
                    (quote! { #c::types::Continue< }).to_tokens(tokens);
                    (0..*n).for_each(|_| (quote! { #c::types::S< }).to_tokens(tokens));
                    (quote! { #c::types::Z }).to_tokens(tokens);
                    (0..=*n).for_each(|_| (quote! { > }).to_tokens(tokens));
                } else {
                    (quote! { #c::types::Continue }).to_tokens(tokens);
                }
            }
            Type(s) => quote! { #s }.to_tokens(tokens),
        }
    }
}
