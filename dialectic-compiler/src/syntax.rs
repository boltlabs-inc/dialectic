//! The abstract surface syntax for the `Session!` macro, produced by the parser.

use quickcheck::{Arbitrary, Gen};
use quote::{quote, ToTokens};
use std::{
    convert::TryFrom,
    fmt::{self, Display, Formatter},
};
use syn::{parse_quote, Error, Type};
use thunderdome::Index;

use crate::{
    cfg::{Cfg, Ir},
    target::Target,
    CompileError, Spanned,
};

/// A shim for parsing the root level of a macro invocation, so that a session type may be written
/// as a block w/o an extra layer of braces.
#[derive(Debug, Clone)]
pub struct Invocation {
    /// The spanned syntax representing a full invocation of the macro.
    pub syntax: Spanned<Syntax>,
}

/// The surface syntax for a macro invocation: a single statement-like item, or a block of them.
///
/// While the [`Target`] of the compiler (and its corresponding types in the Dialectic library) have
/// continuations for almost every expression, the surface syntax is not in continuation passing
/// style, instead encoding sequences of operations using the `;` operator within blocks.
#[derive(Debug, Clone)]
pub enum Syntax {
    /// Syntax: `recv T`.
    Recv(Type),
    /// Syntax: `send T`.
    Send(Type),
    /// Syntax: `call T` or `call { ... }`.
    Call(Box<Spanned<Syntax>>),
    /// Syntax: `choose { _0 => ..., ... }`.
    Choose(Vec<Spanned<Syntax>>),
    /// Syntax: `offer { _0 => ..., ... }`.
    Offer(Vec<Spanned<Syntax>>),
    /// Syntax: `split { <- ..., -> ... }`.
    Split(Box<Spanned<Syntax>>, Box<Spanned<Syntax>>),
    /// Syntax: `loop { ... }` or `'label loop { ... }`.
    Loop(Option<String>, Box<Spanned<Syntax>>),
    /// Syntax: `break` or `break 'label`.
    Break(Option<String>),
    /// Syntax: `continue` or `continue 'label`.
    Continue(Option<String>),
    /// Syntax: `{ ... }`
    Block(Vec<Spanned<Syntax>>),
    /// Syntax: `T`.
    Type(Type),
}

impl Display for Syntax {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Syntax::*;
        let keyword = match self {
            Recv(_) => "recv",
            Send(_) => "send",
            Call(_) => "call",
            Choose(_) => "choose",
            Offer(_) => "offer",
            Split(_, _) => "split",
            Loop(_, _) => "loop",
            Break(_) => "break",
            Continue(_) => "continue",
            Block(_) | Type(_) => "",
        };
        match self {
            Recv(ty) | Send(ty) | Type(ty) => {
                write!(f, "{} ", keyword)?;
                write!(f, "{}", ty.to_token_stream())?;
            }
            Call(callee) => {
                write!(f, "{} ", keyword)?;
                if matches!(callee.inner, Type(_) | Block(_)) {
                    write!(f, "{}", callee.inner)?;
                } else {
                    write!(f, "{{ {} }}", callee.inner)?;
                }
            }
            Choose(choices) | Offer(choices) => {
                write!(f, "{} ", keyword)?;
                write!(f, "{{ ")?;
                for (i, Spanned { inner: choice, .. }) in choices.iter().enumerate() {
                    write!(f, "_{} => {},", i, choice)?;
                }
                write!(f, " }}")?;
            }
            Split(tx, rx) => {
                write!(f, "{} ", keyword)?;
                write!(f, "{{ -> {}, <- {}, }}", tx.inner, rx.inner)?;
            }
            Loop(label, body) => {
                if let Some(label) = label {
                    write!(f, "'{}: ", label)?;
                }
                write!(f, "{} ", keyword)?;
                write!(f, "{{ {} }}", body.inner)?;
            }
            Break(label) | Continue(label) => {
                write!(f, "{}", keyword)?;
                if let Some(label) = label {
                    write!(f, " '{}", label)?;
                }
            }
            Block(statements) => {
                write!(f, "{{ ")?;
                for Spanned { inner, .. } in statements {
                    write!(f, "{}; ", inner)?;
                }
                write!(f, "}}")?;
            }
        };
        Ok(())
    }
}

impl Arbitrary for Syntax {
    fn arbitrary(g: &mut Gen) -> Self {
        use Syntax::*;

        fn arbitrary_in_env(g: &mut Gen, env: &mut Vec<String>) -> Syntax {
            let types: &[syn::Type] = &[parse_quote!(())];

            todo!()
        }

        arbitrary_in_env(g, &mut Vec::new())
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        quickcheck::empty_shrinker()
    }
}

impl Syntax {
    /// Construct a [`Syntax::Recv`] from a string representing a type.
    ///
    /// # Panics
    ///
    /// If the type does not parse correctly, this panics.
    pub fn recv(ty: &str) -> Self {
        Syntax::Recv(syn::parse_str(ty).unwrap())
    }

    /// Construct a [`Syntax::Send`] from a string representing a type.
    ///
    /// # Panics
    ///
    /// If the type does not parse correctly, this panics.
    pub fn send(ty: &str) -> Self {
        Syntax::Send(syn::parse_str(ty).unwrap())
    }

    /// Construct a [`Syntax::Call`] from its inner callee.
    pub fn call(callee: impl Into<Spanned<Syntax>>) -> Self {
        Syntax::Call(Box::new(callee.into()))
    }

    /// Construct a [`Syntax::Loop`] from its (optional) label and its body.
    pub fn loop_(label: Option<String>, body: impl Into<Spanned<Syntax>>) -> Self {
        Syntax::Loop(label, Box::new(body.into()))
    }

    /// Construct a [`Syntax::Type`] from a string representing a type.
    ///
    /// # Panics
    ///
    /// If the type does not parse correctly, this panics.
    pub fn type_(ty: &str) -> Self {
        Syntax::Type(syn::parse_str(ty).unwrap())
    }
}

impl Spanned<Syntax> {
    /// Compile a spanned syntax tree into either a representation of a valid session type
    /// [`Target`], or an [`Error`].
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
                let _ = env.pop();

                // Set the continuation for the loop if it doesn't end in a `Break` or `Continue`
                match cfg[tail].expr {
                    Ir::Break(_) | Ir::Continue(_) => {}
                    _ => cfg[tail].next = Some(cfg.singleton(Ir::Continue(0))),
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
