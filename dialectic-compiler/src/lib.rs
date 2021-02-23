use {proc_macro2::Span, thiserror::Error};

mod cfg;
pub mod parse;
pub mod syntax;
pub mod target;

pub use crate::{
    syntax::{Invocation, Syntax},
    target::Target,
};

/// A compilation error due to invalid (but parseable) input in the surface macro syntax.
#[derive(Error, Debug, Clone)]
pub enum CompileError {
    #[error("label name `'{0}` shadows a label name that is already in scope")]
    ShadowedLabel(String),
    #[error("undeclared label `'{0}`")]
    UndeclaredLabel(String),
    #[error("cannot `continue` outside of a loop")]
    ContinueOutsideLoop,
    #[error("cannot `break` outside of a loop")]
    BreakOutsideLoop,
    #[error("any code following this statement is unreachable")]
    FollowingCodeUnreachable,
    #[error("unreachable statement")]
    UnreachableStatement,
}

#[derive(Debug, Clone, Copy)]
/// A thing attached to some `Span` that tracks its origin in the macro invocation.
pub struct Spanned<T> {
    pub inner: T,
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
        let send = cfg.send("i64");
        let recv = cfg.recv("i64");
        let continue0 = cfg.singleton(Ir::Continue(0));
        cfg[send].next = Some(continue0);
        let continue1 = cfg.singleton(Ir::Continue(1));
        cfg[recv].next = Some(continue1);
        let choose_opts = vec![send, recv];
        let choose = cfg.singleton(Ir::Choose(choose_opts));
        let client_tally = cfg.singleton(Ir::Loop(choose));

        let break0 = cfg.singleton(Ir::Break(0));
        let send = cfg.send("Operation");
        cfg[send].next = Some(client_tally);
        let choose_opts = vec![break0, send];
        let choose = cfg.singleton(Ir::Choose(choose_opts));
        let client = cfg.singleton(Ir::Loop(choose));

        cfg.eliminate_breaks(client);
        let s = format!("{}", cfg.to_target(client).unwrap());
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<_1>>)>>>)>>");
    }

    #[test]
    fn tally_client_cfg_call() {
        let mut cfg = Cfg::new();
        let break0 = cfg.singleton(Ir::Break(0));
        let send = cfg.send("Operation");
        let callee = cfg.type_("ClientTally");
        let call = cfg.singleton(Ir::Call(callee));
        cfg[send].next = Some(call);
        let continue0 = cfg.singleton(Ir::Continue(0));
        cfg[call].next = Some(continue0);
        let choose_opts = vec![break0, send];
        let choose = cfg.singleton(Ir::Choose(choose_opts));
        let client = cfg.singleton(Ir::Loop(choose));

        cfg.eliminate_breaks(client);
        let s = format!("{}", cfg.to_target(client).unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }
}
