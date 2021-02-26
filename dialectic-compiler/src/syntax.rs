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

    /// Converts surface [`Syntax`] to a control-flow graph intermediate representation, suitable
    /// for further analysis and error reporting.
    fn to_cfg<'a>(
        &'a self,
        cfg: &mut Cfg,
        env: &mut Vec<(&'a Option<String>, Index)>,
    ) -> (Option<Index>, Option<Index>) {
        // Helper function for converting break and continue nodes to CFG nodes. This is here
        // because the cases for `Break` and `Continue` are 100% identical minus the emitted errors
        // and constructed IR node variants; so this function extracts that logic and parameterizes
        // it over the error emitted in the case that the jump node exists outside of a loop as well
        // as the constructor (which is always of the type `fn(Index) -> Ir`.)
        let mut convert_jump_to_cfg = |label: &Option<String>,
                                       outside_loop_error: CompileError,
                                       constructor: fn(Index) -> Ir|
         -> (Option<Index>, Option<Index>) {
            let node = match env.last() {
                // When not inside a loop at all:
                None => cfg.create_error(outside_loop_error, self.span),
                // When inside a loop:
                Some((_, index)) => match label {
                    // With an unlabeled break/continue:
                    None => cfg.spanned(constructor(*index), self.span),
                    // With a labeled break/continue:
                    Some(label) => {
                        let found_index = env.iter().rev().find(|&l| l.0.as_ref() == Some(label));
                        match found_index {
                            // If label cannot be found:
                            None => cfg.create_error(
                                CompileError::UndeclaredLabel(label.clone()),
                                self.span,
                            ),
                            // If label is found:
                            Some((_, index)) => cfg.spanned(constructor(*index), self.span),
                        }
                    }
                },
            };

            (Some(node), Some(node))
        };

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
            Continue(label) => {
                return convert_jump_to_cfg(label, CompileError::ContinueOutsideLoop, Ir::Continue)
            }
            Break(label) => {
                return convert_jump_to_cfg(label, CompileError::BreakOutsideLoop, Ir::Break)
            }
            Loop(maybe_label, body) => {
                // Constructing a loop node is a kind of fixed-point operation, where
                // any break and continue nodes within need to know the index of their
                // respective loop node. To solve this, we create an empty loop and use
                // its index to hold the places of the data any break or continue needs,
                // and then assign the correct `Ir::Loop(head)` value later.
                let ir_node = cfg.spanned(Ir::Loop(None), self.span);

                // Convert the body in the environment with this loop label
                env.push((maybe_label, ir_node)); // NOTE: this is the only `push` in this function!
                let head = body.to_cfg(cfg, env).0;
                let _ = env.pop(); // NOTE: this is the only `pop` in this function!

                // Close out that fixed point; the loop block is now correctly built.
                cfg[ir_node].expr = Ir::Loop(head);

                // Check to ensure the environment does not already contain this label
                let node =
                    if maybe_label.is_some() && env.iter().any(|scope| scope.0 == maybe_label) {
                        cfg.create_error(
                            CompileError::ShadowedLabel(maybe_label.clone().unwrap()),
                            self.span,
                        )
                    } else {
                        ir_node
                    };

                // Because we already know the index we must return and cannot allow another
                // index to be created for this node, we have to early-return here.
                return (Some(node), Some(node));
            }
            Block(statements) => {
                // Connect the continuations of each statement in the block to the subsequent
                // statement in the block, by inserting them in reverse order into the graph
                let mut next_head = None;
                let mut next_tail = None;
                for stmt in statements.iter().rev() {
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
