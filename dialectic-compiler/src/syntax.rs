//! The abstract surface syntax for the `Session!` macro, produced by the parser.

use {
    syn::{Error, Type},
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
        cfg.resolve_scopes(head);
        cfg.report_dead_code(head);
        cfg.to_target(head)
    }

    fn to_cfg<'a>(
        &'a self,
        cfg: &mut Cfg,
        env: &mut Vec<(&'a Option<String>, Index)>,
    ) -> (Option<Index>, Option<Index>) {
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
            Continue(label) => match env.last() {
                None => Ir::Error(CompileError::ContinueOutsideLoop, None),
                Some((_, index)) => match label {
                    None => Ir::Continue(*index),
                    Some(label) => match env.iter().rev().find(|&l| l.0.as_ref() == Some(label)) {
                        None => Ir::Error(CompileError::UndeclaredLabel(label.clone()), None),
                        Some((_, index)) => Ir::Continue(*index),
                    },
                },
            },
            Break(label) => match env.last() {
                None => Ir::Error(CompileError::BreakOutsideLoop, None),
                Some((_, index)) => match label {
                    None => Ir::Break(*index),
                    Some(label) => match env.iter().rev().find(|&l| l.0.as_ref() == Some(label)) {
                        None => Ir::Error(CompileError::UndeclaredLabel(label.clone()), None),
                        Some((_, index)) => Ir::Break(*index),
                    },
                },
            },
            Loop(maybe_label, body) => {
                // Constructing a loop node is a kind of fixed-point operation, where
                // any break and continue nodes within need to know the index of their
                // respective loop node. To solve this, we create an empty loop and use
                // its index to hold the places of the data any break or continue needs,
                // and then assign the correct `Ir::Loop(head)` value later.
                let ir_node = cfg.spanned(Ir::Loop(None), self.span);

                // Convert the body in the environment with this loop label
                env.push((maybe_label, ir_node));
                let head = body.to_cfg(cfg, env).0;
                let _ = env.pop();

                // Close out that fixed point; the loop block is now correctly built.
                cfg[ir_node].expr = Ir::Loop(head);

                // Check to ensure the environment does not already contain this label
                if maybe_label.is_some() && env.iter().any(|scope| scope.0 == maybe_label) {
                    Ir::Error(
                        CompileError::ShadowedLabel(maybe_label.clone().unwrap()),
                        Some(ir_node),
                    )
                } else {
                    // Like the `Block` clause, we've already calculated and used this
                    // node's index, so we don't want to construct a new node from it.
                    // So, we need to return here, before the code after the match
                    // constructs a new `Index` from a returned `Ir` variant.
                    return (Some(ir_node), Some(ir_node));
                }
            }
            Block(stmts) => {
                let mut next_head = None;
                let mut next_tail = None;
                for stmt in stmts.iter().rev() {
                    let (head, tail) = stmt.to_cfg(cfg, env);
                    next_tail = next_tail.or(tail);

                    if let Some(tail) = tail {
                        cfg[tail].next = next_head
                    }

                    next_head = head;
                }

                // Only case where we return a differing head and tail, since a `Block` is the only
                // expression to be compiled into multiple nodes
                return (next_head, next_tail);
            }
        };

        let node = cfg.spanned(ir, self.span);
        (Some(node), Some(node))
    }
}
