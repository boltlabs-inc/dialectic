//! The abstract surface syntax for the `Session!` macro, produced by the parser.

use {
    proc_macro2::TokenStream,
    quote::{format_ident, quote_spanned, ToTokens},
    syn::{Error, Lifetime, Type},
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
    /// Syntax: `split { -> ..., <- ... }`.
    Split {
        /// The transmit-only half.
        tx_only: Box<Spanned<Syntax>>,
        /// The receive-only half.
        rx_only: Box<Spanned<Syntax>>,
    },
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
    pub fn to_session(&self) -> Result<Spanned<Target>, Error> {
        let mut cfg = Cfg::new();
        let head = self.to_cfg(&mut cfg, &mut Vec::new()).0;
        cfg.resolve_scopes(head);
        cfg.report_dead_code(head);
        cfg.generate_target(head)
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
            Split { tx_only, rx_only } => {
                let tx_only = tx_only.to_cfg(cfg, env).0;
                let rx_only = rx_only.to_cfg(cfg, env).0;
                Ir::Split { tx_only, rx_only }
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
                // Constructing a loop node is a kind of fixed-point operation, where any break and
                // continue nodes within need to know the index of their respective loop node. To
                // solve this, we create an empty loop and use its index to hold the places of the
                // data any break or continue needs, and then assign the correct `Ir::Loop(head)`
                // value later.
                let ir_node = cfg.spanned(Ir::Loop(None), self.span);

                // Convert the body in the environment with this loop label
                env.push((maybe_label, ir_node)); // NOTE: this is the only `push` in this function!
                let head = body.to_cfg(cfg, env).0;
                let _ = env.pop(); // NOTE: this is the only `pop` in this function!

                // Close out that fixed point; the loop block is now correctly built.
                cfg[ir_node].expr = Ir::Loop(head);

                // Check to ensure the environment does not already contain this label. If it does,
                // keep going, but insert an error on the relevant loop node.
                if maybe_label.is_some() && env.iter().any(|scope| scope.0 == maybe_label) {
                    cfg.insert_error_at(
                        ir_node,
                        CompileError::ShadowedLabel(maybe_label.clone().unwrap()),
                    );
                }

                // Because we already know the index we must return and cannot allow another index
                // to be created for this node, we have to early-return here.
                return (Some(ir_node), Some(ir_node));
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

impl Spanned<Syntax> {
    /// Convenience function for converting a `Spanned<Syntax>` into a `TokenStream` via
    /// [`Spanned::to_tokens_with`].
    pub fn to_token_stream_with<F: ?Sized>(&self, add_optional: &mut F) -> TokenStream
    where
        F: FnMut() -> bool,
    {
        let mut acc = TokenStream::new();
        self.to_tokens_with(add_optional, &mut acc);
        acc
    }

    /// Convert a `Spanned<Syntax>` into tokens and append them to the provided token stream, with a
    /// predicate to determine whether or not optional syntax should be added or not.
    ///
    /// The added optional syntax predicate is an `FnMut` closure so that we can randomly choose to
    /// add or not add optional trailing commas and similar during testing.
    pub fn to_tokens_with<F: ?Sized>(&self, mut add_optional: &mut F, tokens: &mut TokenStream)
    where
        F: FnMut() -> bool,
    {
        use Syntax::*;

        let sp = self.span;

        let choice_arms_to_tokens =
            |add_optional: &mut dyn FnMut() -> bool, arms: &[Spanned<Syntax>]| -> TokenStream {
                let mut acc = TokenStream::new();
                for (i, choice) in arms.iter().enumerate() {
                    let idx = format_ident!("_{}", i, span = sp);
                    quote_spanned!(sp=> #idx => ).to_tokens(&mut acc);
                    choice.to_tokens_with(add_optional, &mut acc);
                    if i < arms.len() - 1 || add_optional() {
                        quote_spanned!(sp=> ,).to_tokens(&mut acc);
                    }
                }
                acc
            };

        match &self.inner {
            Recv(t) => quote_spanned! {sp=> recv #t },
            Send(t) => quote_spanned! {sp=> send #t },
            Call(callee) => {
                let mut acc = TokenStream::new();
                quote_spanned!(sp=> call ).to_tokens(&mut acc);

                if matches!(callee.inner, Block(_) | Type(_)) {
                    callee.to_tokens_with(add_optional, &mut acc);
                } else {
                    let callee_tokens = callee.to_token_stream_with(add_optional);
                    quote_spanned!(sp=> { #callee_tokens }).to_tokens(&mut acc);
                }

                acc
            }
            Split { tx_only, rx_only } => {
                let tx = tx_only.to_token_stream_with(add_optional);
                let rx = rx_only.to_token_stream_with(add_optional);
                quote_spanned! {sp=> split { -> #tx, <- #rx, } }
            }
            Choose(choices) => {
                let arms = choice_arms_to_tokens(&mut add_optional, choices);
                quote_spanned! {sp=> choose { #arms } }
            }
            Offer(choices) => {
                let arms = choice_arms_to_tokens(&mut add_optional, choices);
                quote_spanned! {sp=> offer { #arms } }
            }
            Loop(None, body) => {
                let body_tokens = body.to_token_stream_with(add_optional);
                quote_spanned! {sp=> loop #body_tokens }
            }
            Loop(Some(label), body) => {
                let lt = Lifetime::new(&format!("'{}", label), sp);
                let body_tokens = body.to_token_stream_with(add_optional);
                quote_spanned! {sp=> #lt: loop #body_tokens }
            }
            Break(None) => quote_spanned! {sp=> break },
            Break(Some(label)) => {
                let lt = Lifetime::new(&format!("'{}", label), sp);
                quote_spanned! {sp=> break #lt }
            }
            Continue(None) => quote_spanned! {sp=> continue },
            Continue(Some(label)) => {
                let lt = Lifetime::new(&format!("'{}", label), sp);
                quote_spanned! {sp=> continue #lt }
            }
            Block(stmts) => {
                let mut acc = TokenStream::new();

                // We want to separate the last statement from the rest so that we can decide
                // whether or not to put an (optional) terminator on it.
                if let Some((last, up_to_penultimate)) = stmts.split_last() {
                    for stmt in up_to_penultimate {
                        stmt.to_tokens_with(add_optional, &mut acc);

                        // If the statement is a call w/ a block argument then a semicolon is
                        // optional, rather than required.
                        let is_call_of_block = matches!(
                            &stmt.inner,
                            Call(callee) if matches!(&callee.inner, Block(_)),
                        );

                        // If the statement is a block, split, offer, choose, or loop construct,
                        // then it ends with a block/brace, and therefore a semicolon is optional,
                        // rather than required.
                        let ends_with_block = matches!(
                            &stmt.inner,
                            Block(_) | Split { .. } | Offer(_) | Choose(_) | Loop(_, _),
                        );

                        if !(is_call_of_block || ends_with_block) || add_optional() {
                            quote_spanned!(sp=> ;).to_tokens(&mut acc);
                        }
                    }

                    last.to_tokens_with(add_optional, &mut acc);

                    if add_optional() {
                        quote_spanned!(sp=> ;).to_tokens(&mut acc);
                    }
                }

                quote_spanned!(sp=> { #acc })
            }
            Type(ty) => quote_spanned! {sp=> #ty },
        }
        .to_tokens(tokens);
    }
}

impl ToTokens for Spanned<Syntax> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.to_tokens_with(&mut || false, tokens);
    }
}

#[cfg(feature = "quickcheck")]
mod tests {
    use super::*;

    use {
        quickcheck::{Arbitrary, Gen},
        syn::parse_quote,
    };

    #[derive(Debug, Clone)]
    struct Label(String);

    impl Arbitrary for Label {
        fn arbitrary(g: &mut Gen) -> Self {
            let s = *g
                .choose(&[
                    "foo", "bar", "baz", "qux", "quux", "corge", "grault", "garply", "waldo",
                    "fred", "plugh", "xyzzy", "thud", "wibble", "wobble", "wubble", "flob",
                ])
                .unwrap();
            Label(s.to_owned())
        }
    }

    /// Currently, we have a bit of a limitation on the arbitrary impl for syntax. The first part of
    /// this is that it's annoying to generate valid labels for lifetimes, so for now we're using a
    /// set of known valid lifetime names (see the [`Label`] type.) The second is that generating
    /// valid Rust types is somewhat nontrivial and the syn crate does not have a way to ask it to
    /// implement `Arbitrary` for its `Type` AST node. So currently we just use the unit type `()`
    /// wherever we may need to place a type.
    impl Arbitrary for Spanned<Syntax> {
        fn arbitrary(g: &mut Gen) -> Self {
            use Syntax::*;
            // Ensure that the size of the syntax tree strictly decreases as we progress. This makes
            // sure that the generator size parameter decreases by one, down to a minimum of 1 (if
            // the generator size becomes zero, quickcheck will panic.)
            let g = &mut Gen::new(g.size().max(2) - 1);
            let syntax = match *g
                .choose(&[
                    "recv", "send", "call", "choose", "offer", "split", "loop", "break",
                    "continue", "block", "type",
                ])
                .unwrap()
            {
                "recv" => Recv(parse_quote!(())),
                "send" => Send(parse_quote!(())),
                // During parsing, call nodes are only allowed to contain blocks or types, and
                // anything else triggers a parse-time error. We want to generate something which
                // could conceivably come from a valid parse operation, so we must limit our call
                // nodes to be either blocks or types.
                "call" => match *g.choose(&["block", "type"]).unwrap() {
                    "block" => Call(Box::new(Spanned::from(Block(Arbitrary::arbitrary(g))))),
                    "type" => Type(parse_quote!(())),
                    other => unreachable!("{}", other),
                },
                "choose" => Choose(Arbitrary::arbitrary(g)),
                "offer" => Offer(Arbitrary::arbitrary(g)),
                "split" => Split {
                    tx_only: Arbitrary::arbitrary(g),
                    rx_only: Arbitrary::arbitrary(g),
                },
                "loop" => Loop(
                    <Option<Label>>::arbitrary(g).map(|l| l.0),
                    Box::new(Spanned::from(Block(Arbitrary::arbitrary(g)))),
                ),
                "break" => Break(<Option<Label>>::arbitrary(g).map(|l| l.0)),
                "continue" => Continue(<Option<Label>>::arbitrary(g).map(|l| l.0)),
                "block" => Block(Arbitrary::arbitrary(g)),
                "type" => Type(parse_quote!(())),
                other => unreachable!("{}", other),
            };

            Spanned::from(syntax)
        }

        // We need to be careful here not to shrink the labels because shrinking a string can result
        // in the empty string, which will panic when we try to construct a syn `Lifetime` from it.
        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            use Syntax::*;
            let span = self.span;
            let v = match &self.inner {
                Recv(_) | Send(_) | Type(_) | Break(None) | Continue(None) => vec![],
                Call(callee) => callee.shrink().map(Call).collect(),
                Split { tx_only, rx_only } => tx_only
                    .shrink()
                    .flat_map(|tx_shrunk| {
                        rx_only.shrink().map(move |rx_shrunk| Split {
                            tx_only: tx_shrunk.clone(),
                            rx_only: rx_shrunk,
                        })
                    })
                    .collect(),
                Choose(choices) => choices.shrink().map(Choose).collect(),
                Offer(choices) => choices.shrink().map(Offer).collect(),
                Loop(label, body) => body
                    .shrink()
                    .map(|body_shrunk| Loop(label.clone(), body_shrunk))
                    .chain(label.as_ref().map(|_| Loop(None, body.clone())))
                    .collect(),
                Block(stmts) => stmts.shrink().map(Block).collect(),
                Break(Some(_)) => vec![Break(None)],
                Continue(Some(_)) => vec![Continue(None)],
            };

            Box::new(v.into_iter().map(move |inner| Spanned { inner, span }))
        }
    }
}
