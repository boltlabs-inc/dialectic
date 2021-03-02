#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications)]
#![warn(future_incompatible)]
#![warn(unused)]
#![forbid(broken_intra_doc_links)]

//! This is a crate. FIXME.

use {proc_macro2::Span, thiserror::Error};

mod cfg;
mod flow;
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
    #[error("loop is unproductive (does nothing before always continuing)")]
    UnproductiveLoop,
    /// Error resulting from any `continue` or `break` which produce an unproductive loop.
    #[error("unproductive jump causes an unproductive loop")]
    UnproductiveJump,
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

        let s = format!("{}", cfg.to_target(Some(client)).unwrap());
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<_1>>)>>>)>>");
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

        let s = format!("{}", cfg.to_target(Some(client)).unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }
}
