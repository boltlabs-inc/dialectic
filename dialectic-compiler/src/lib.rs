/*!
[![Rust](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml/badge.svg)](https://github.com/boltlabs-inc/dialectic/actions/workflows/rust.yml)
![license: MIT](https://img.shields.io/github/license/boltlabs-inc/dialectic)
[![crates.io](https://img.shields.io/crates/v/dialectic-compiler)](https://crates.io/crates/dialectic-compiler)
[![docs.rs documentation](https://docs.rs/dialectic-compiler/badge.svg)](https://docs.rs/dialectic-compiler)

Contains functionality for compiling the language of Dialectic's `Session!` macro into an actual Rust type. This crate is considered an internal implementation detail and none of its API is subject to semantic versioning rules. The compilation process is done via transforming the syntax gradually across three representations:

- [`syntax::Syntax`] - the "surface-level" abstract syntax tree. This is what gets parsed from the tokenstream provided by the proc macro interface.
- [`cfg::Cfg`] - a control flow graph representation, which is gradually transformed by semantics-preserving passes until it takes on a form more suitable for emitting to the target.
- [`target::Target`] - the "target language" syntax tree, which directly maps to Dialectic session types.

# `Syntax` AST transformations

After parsing, the `Syntax` AST only really undergoes one transformation, when it is converted to the control flow graph representation.

## Conversion to CFG - [`Syntax::to_cfg`]

During this conversion, we resolve labels in the AST and ensure that all `break` and `continue` constructs refer to valid nodes and emit errors for malformed loop nodes and such. Also, please note this method is often referred to as `Syntax::to_cfg` but the method is *actually* implemented on `Spanned<Syntax>`.

### Errors

This pass may emit several errors:

- [`CompileError::UndeclaredLabel`] - emitted when we find a reference to a label that's not in scope
- [`CompileError::ShadowedLabel`] - emitted when two loops in the same scope have the same label
- [`CompileError::ContinueOutsideLoop`] - emitted when we find a `Continue` in an empty environment
- [`CompileError::BreakOutsideLoop`] - emitted when we find a `Break` in an empty environment

# [`Cfg`] transformations

The CFG undergoes only one really crucial explicit transform before undergoing flow analysis, error reporting, and then finally lowering to the `Target`. The rough outline of CFG processing looks like this:

- Scope resolution and implicit continue insertion
- Control flow analysis
- Reachability analysis, using the control flow analysis output
- Dead code analysis - report unreachable code, which will never be emitted by the compiler
- Target generation, break elimination, and loop productivity analysis

## Scope resolution - [`Cfg::resolve_scopes`]

The scope resolution pass is implemented in the [`Cfg::resolve_scopes`] method. It traverses the CFG from a given root node, and converts "implicit" continuations to "explicit" continuations. Because implicit continues are another sort of "implicit" continuation represented by the absence of a continuation in the scope of a loop body, we also insert them here. There is a chance, however, that a machine-generated `Continue` node of this sort will actually be unreachable code, which would trigger an error during dead code analysis; because of that, machine generated `Continues` are marked with the `machine_generated` field of `CfgNode`, and dead code reporting is configured to ignore them.

After the scope resolution pass, there are three critical invariants we can use:

- [`Ir::Choose`] and [`Ir::Offer`] nodes will never have a continuation (their `node.next` fields will always be `None`).
- [`Ir::Loop`] nodes will always have a nonempty body (the `Option<Index>` inside the [`Ir::Loop`] variant will always be `Some(body)`).
- Paths through the body of an [`Ir::Loop`] node will always terminate in an [`Ir::Break`] or an [`Ir::Continue`].

Most other passes will make use these assumptions.

The scope resolution algorithm looks something like this:

```text
fn resolve_scopes(implicit_scope, node) {
    if the node is a Recv, Send, Type, Break, Continue, Call, Split, or Error {
        // The implicit continuation of the callee of a Call node or arm of a Split node is always
        // Done, because we want to to run it until Done rather than to continue afterwards, as the
        // Call/Split node takes care of the continuation.
        if the node is a Call(callee) {
            resolve_scopes(None, callee);
        }

        if the node is a Split(tx, rx) {
            resolve_scopes(None, tx);
            resolve_scopes(None, rx);
        }

        // If this node has an explicit continuation, then we follow it and continue visiting! If it
        // doesn't, then we assign the implicit continuation in its scope to become its new explicit
        // continuation.
        match node.next {
            Some(next) => resolve_scopes(implicit_cont, next),
            None => node.next = implicit_scope,
        }
    } else if the node is a Choose(choices) or Offer(choices) {
        // Remove the continuation from the node if present; we inline it into all arms of the //
        // Choose/Offer.
        let cont = match node.next.take() {
            // If we find an implicit continuation, then it's the new "top of the continuation
            // scope" for this node's arms. In order to properly inline the outer scope's implicit
            // continuation, we visit the new implicit continuation w/ the previous one in order to
            // cause it to be inlined in the correct places as well.
            Some(next) => {
                resolve_scopes(implicit_cont, next);
                Some(next)
            }
            // If this node doesn't have an implicit continuation, then there's no need to worry
            // about inlining the outer scope's implicit continuation into it, as we can just inline
            // the outer implicit continuation into every arm instead.
            None => implicit_cont,
        };

        for choice in choices {
            resolve_scopes(cont, choice);
        }

        // We never follow or reassign a Choose/Offer node's implicit continuation, because it's
        // been inlined into all its arms already.
    } else if the node is a Loop(body) {
        // Inside a loop body, the absence of an explicit continuation doesn't imply following the
        // implicit continuation held by the scope outside the loop (the continuation of the loop
        // block) - instead, it implies continuing the loop! So to handle that, we visit the body of
        // the loop with its implicit continuation set to a Continue node we generate, targeted at
        // the loop node itself.
        let continue0 = Continue(node);
        continue0.machine_generated = true;
        resolve_scopes(continue0, body);
    }
}

// We begin w/ an empty implicit continuation (the Done continuation) and start traversing from the
// root.
resolve_scopes(None, root);
```

### Errors

This pass never emits any errors.

## Dead code reporting - [`Cfg::report_dead_code`]

The dead core reporting pass itself is responsible for running the flow analysis and reachability analysis. After these are run, it traverses the control flow graph from the root, looking for nodes which the flow analysis considers "impassable" (control flow does not proceed to their continuations) but which still have a continuation. This gives us the location of all the points in the program which consist of reachable code immediately adjacent to unreachable code, which is much nicer for the user to see errors of rather than errors for *every* unreachable node, of which there may be many in the same flow path.

The dead code reporting algorithm looks something like this:

```text
fn report_dead_code(node) {
    // We want to follow every child node except for the node's continuation. If we did follow the
    // continuation, we would end up reporting every unreachable node instead of just the
    // unreachable nodes on the boundary between reachable and unreachable code.
    for child in all children of node {
        report_dead_code(child);
    }

    if let Some(cont) = node.next {
        if node is passable {
            report_dead_code(cont);
        } else if !cont.machine_generated && cont is not reachable {
            emit unreachable code errors;
        }
    }
}

report_dead_code(node);
```

### Errors

This pass emits two types of errors:

- [`CompileError::FollowingCodeUnreachable`] - emitted on a reachable node which has an unreachable continuation
- [`CompileError::UnreachableStatement`] - emitted on an unreachable node that is the continuation of a reachable node

## Lowering to target/target code generation - [`Cfg::generate_target`]

The code generation/lowering pass also performs loop productivity analysis. An unproductive loop is technically a valid surface language program but will cause the Rust typechecker as of Rust 1.51 to infinitely loop and generate an error which is very difficult for new users to decipher. To prevent this, we detect them and report them instead of compiling a technically valid session type which will not compile as valid Rust.

Loop productivity analysis is conducted during the target generation pass because it is a syntactic property of the target language itself.

For most nodes, generating the corresponding target code is trivial - `Send`, `Recv`, `Call`, `Split`, `Choose`, and `Offer` all map very directly to their [`Target`] counterparts. The complex components are `Loop`, `Continue`, `Type`, `Break`, and `Error`:

- `Continue`s must be converted from directly referencing their target to referencing the correct DeBruijn index of the target loop. For this purpose, we keep a loop environment as a stack which is pushed to when entering a loop node and popped when exiting. Calculating the DeBruijn index corresponding to a loop is done with a linear search on the loop environment looking for the position of the matching loop in the loop environment.
- `Break` has no corresponding representation in the `Target`; instead, it lowered by substituting any reference to it with the continuation of the loop it references. The loop's continuation must also be placed into its own loop environment, because in the `Target`, continuations of a loop are simply parts of a loop which do not end in a `Continue`. This actually makes codegen a little bit more convenient, but should still be noted.
- `Loop` must push its index to the loop environment stack before generating its body, and then pop once its body is generated, in order to ensure the loop environment properly corresponds to the scope in the target AST.
- `Type` in the `Target` has nowhere to put the continuation corresponding to its next pointer. In the `Target`, a `Type` is run until it is `Done`. So there are two cases; if the `Type`'s continuation is `None` (the "done" continuation) we can just emit the type directly as a [`Target::Type`] node; if it is `Some`, we must emit a [`Target::Then`] which sequences the `Type`'s continuation after it.
- `Error` has no corresponding representation in the `Target`, and is substituted with its continuation during codegen. While they are allowed *during* codegen, a codegen pass which encounters an `Error` node will already have encountered at least one corresponding emitted [`CompileError`], and the resulting target AST will not be returned from `Cfg::generate_target`.

The codegen algorithm looks something like this:

```text
fn generate_target(loop_env, maybe_node) {
    if maybe_node is None {
        // If the node is empty, that's the "done" continuation.
        return Done;
    } else if maybe_node is Some(node) {
        if node is NOT Loop, Continue, Break {
            // Note that the current loop's target representation contains something which is not
            // another Loop in between itself and its `Continue` if present.
            loop_env.mark_productive();
        }

        if node is Recv, Send, Call, Split, Choose, or Offer {
            // Recursively call generate_target on child nodes and convert to the directly
            // corresponding Target.
            return Target:: ...;
        } else if node is Loop(body) {
            loop_env.push(node);
            let body = generate_target(loop_env, body);
            loop_env.pop(node);
            return Target::Loop(body);
        } else if node is Continue(jump_target) {
            let debruijn_index = loop_env.debruijn_index_of(jump_target);
            // If we've hit a continue, we can know whether or not the corresponding loop is
            // productive or not, and emit an error if not.
            if !loop_env.productive_for_continue(debruijn_index) {
                // Emit an error for the unproductive loop, and for the unproductive continue *if*
                // it is not machine generated
                ...
            }
            return Target::Continue(debruijn_index);
        } else if node is Break(jump_target) {
            // For a break, return the result of generating the target form of its corresponding
            // loop's continuation.
            return generate_target(loop_env, jump_target.next);
        } else if node is Type(ty) {
            // If the continuation is "done", then we don't need to emit a Then.
            if node.next is None {
                return Target::Type(ty);
            } else {
                let cont = generate_target(loop_env, node.next);
                return Target::Then(Target::Type(ty), );
            }
        } else if node is Error {
            // Just keep going so we can collect more loop productivity errors.
            return generate_target(loop_env, node.next);
        }
    }
}
```

### Errors

This pass emits two types of errors:

- [`CompileError::UnproductiveLoop`] - emitted on a loop node which is found to be unproductive
- [`CompileError::UnproductiveContinue`] - emitted on a non-machine-generated continue node which causes a loop to be unproductive

# [`Target`] transformations

At current, the target AST does not undergo any kind of transform before it is converted very transparently to its destination format (whether that's to be displayed as a string or emitted as a Rust token tree.)

<!-- snip -->

[`Syntax::to_cfg`]: crate::Spanned::to_cfg
[`Cfg`]: crate::cfg::Cfg
[`Cfg::resolve_scopes`]: crate::cfg::Cfg::resolve_scopes
[`Cfg::report_dead_code`]: crate::cfg::Cfg::report_dead_code
[`Cfg::generate_target`]: crate::cfg::Cfg::generate_target
[`Ir`]: crate::cfg::Ir
[`Ir::Offer`]: crate::cfg::Ir::Offer
[`Ir::Choose`]: crate::cfg::Ir::Choose
[`Ir::Loop`]: crate::cfg::Ir::Loop
[`Ir::Break`]: crate::cfg::Ir::Break
[`Ir::Continue`]: crate::cfg::Ir::Continue
*/

#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications)]
#![warn(future_incompatible)]
#![warn(unused)]
#![forbid(broken_intra_doc_links)]

use {
    proc_macro2::Span,
    proc_macro_crate::FoundCrate,
    quote::format_ident,
    std::{
        env,
        fmt::{Display, Formatter},
    },
    syn::{parse_quote, Path},
    thiserror::Error,
};

pub mod cfg;
pub mod flow;
pub mod parse;
pub mod syntax;
pub mod target;

pub use crate::{
    syntax::{Invocation, Syntax},
    target::Target,
};

/// A compilation error due to invalid (but parseable) input in the surface macro syntax.
#[derive(Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CompileError {
    /// Error resulting from `'a loop { ... 'a loop { ... }}`.
    #[error("label name `'{0}` shadows a label name that is already in scope")]
    ShadowedLabel(String),
    /// Error resulting from `continue 'a` or `break 'a` which *are* inside a `loop`, but not inside
    /// any loop with the label `'a`.
    #[error("undeclared label `'{0}`")]
    UndeclaredLabel(String),
    /// Error resulting from any call to `continue` outside of a `loop`.
    #[error("cannot `continue` outside of a loop")]
    ContinueOutsideLoop,
    /// Error resulting from any call to `break` outside of a `loop`.
    #[error("cannot `break` outside of a loop")]
    BreakOutsideLoop,
    /// Error resulting from control flow analysis finding that a statement unconditionally jumps
    /// away from following code.
    #[error("any code following this statement is unreachable")]
    FollowingCodeUnreachable,
    /// Error resulting from control flow analysis finding that a statement can never be reached
    /// because of preceding control flow.
    #[error("unreachable statement")]
    UnreachableStatement,
    /// Error resulting from unproductive loop analysis finding that a loop is unproductive and
    /// cannot be compiled without resulting in typechecker overflow.
    #[error("this `loop` statement is unproductive (it takes no actions before repeating)")]
    UnproductiveLoop,
    /// Error resulting from any `continue` or `break` which produce an unproductive loop.
    #[error("this `continue` statement causes an unproductive `loop`")]
    UnproductiveContinue,
}

#[derive(Debug, Clone, Copy)]
/// A thing attached to some `Span` that tracks its origin in the macro invocation.
pub struct Spanned<T> {
    /// The thing to which the [`Span`] is attached.
    pub inner: T,
    /// The [`Span`] which is attache to the thing.
    pub span: Span,
}

impl<T> From<T> for Spanned<T> {
    fn from(inner: T) -> Self {
        Self {
            inner,
            span: Span::call_site(),
        }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.inner)
    }
}

/// Returns a path prefix which refers to the root module of the dialectic crate, regardless of
/// where it is called (inside dialectic, doctests in dialectic, unit tests in dialectic,
/// integration tests in dialectic, outside dialectic.)
pub fn dialectic_path() -> Path {
    // We need to find the right path where we can reference types in our proc macro. This is a
    // little tricky. There are three cases to consider.
    match proc_macro_crate::crate_name("dialectic") {
        // The first case is that we are in dialectic and compiling dialectic itself, OR we are
        // compiling a dialectic doctest. In this case, we want to use `crate::dialectic`, which
        // will grab the symbol "dialectic" in the crate root. In the case of a doctest, this
        // will result in the extern crate dialectic; in the case of dialectic itself, it will
        // result in a private dummy module called "dialectic", which exists to support macro
        // calls like these and re-exports dialectic::types.
        Ok(FoundCrate::Itself) if env::var("CARGO_CRATE_NAME").as_deref() == Ok("dialectic") => {
            parse_quote!(crate::dialectic)
        }
        // The second case is that we are in an integration test of dialectic. This one's
        // straightforward.
        Ok(FoundCrate::Itself) | Err(_) => parse_quote!(::dialectic),
        // And lastly, the third case: we are in a user's crate. We found the crate with the
        // name `dialectic` and will use that identifier as our crate name, in a similar manner
        // to the second case, prefixed with `::` to ensure it is a "global" path.
        Ok(FoundCrate::Name(name)) => {
            let name_ident = format_ident!("{}", name);
            parse_quote!(::#name_ident)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::*;
    use thunderdome::Index;

    impl Cfg {
        fn send(&mut self, ty: &str) -> Index {
            self.singleton(Ir::Send(syn::parse_str(ty).unwrap()))
        }

        fn recv(&mut self, ty: &str) -> Index {
            self.singleton(Ir::Recv(syn::parse_str(ty).unwrap()))
        }

        fn type_(&mut self, ty: &str) -> Index {
            self.singleton(Ir::Type(syn::parse_str(ty).unwrap()))
        }
    }

    #[test]
    fn tally_client_cfg_direct_subst() {
        let mut cfg = Cfg::new();
        let client_tally = cfg.singleton(Ir::Loop(None));
        let client = cfg.singleton(Ir::Loop(None));

        let send = cfg.send("i64");
        let recv = cfg.recv("i64");
        let continue0 = cfg.singleton(Ir::Continue(client_tally));
        cfg[send].next = Some(continue0);
        let continue1 = cfg.singleton(Ir::Continue(client));
        cfg[recv].next = Some(continue1);
        let choose_opts = vec![Some(send), Some(recv)];
        let choose = cfg.singleton(Ir::Choose(choose_opts));

        cfg[client_tally].expr = Ir::Loop(Some(choose));

        let break0 = cfg.singleton(Ir::Break(client));
        let send = cfg.send("Operation");
        cfg[send].next = Some(client_tally);
        let choose_opts = vec![Some(break0), Some(send)];
        let choose = cfg.singleton(Ir::Choose(choose_opts));

        cfg[client].expr = Ir::Loop(Some(choose));

        let s = format!("{}", cfg.generate_target(Some(client)).unwrap());
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue<0>>, Recv<i64, Continue<1>>)>>>)>>");
    }

    #[test]
    fn tally_client_cfg_call() {
        let mut cfg = Cfg::new();
        let client = cfg.singleton(Ir::Loop(None));
        let break0 = cfg.singleton(Ir::Break(client));
        let send = cfg.send("Operation");
        let callee = cfg.type_("ClientTally");
        let call = cfg.singleton(Ir::Call(Some(callee)));
        cfg[send].next = Some(call);
        let continue0 = cfg.singleton(Ir::Continue(client));
        cfg[call].next = Some(continue0);
        let choose_opts = vec![Some(break0), Some(send)];
        let choose = cfg.singleton(Ir::Choose(choose_opts));

        cfg[client].expr = Ir::Loop(Some(choose));

        let s = format!("{}", cfg.generate_target(Some(client)).unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue<0>>>)>>"
        );
    }
}
