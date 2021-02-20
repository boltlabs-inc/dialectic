use {
    lazy_static::lazy_static,
    proc_macro2::{Span, TokenStream},
    quote::{quote, ToTokens},
    std::{fmt, ops, rc::Rc},
    syn::{spanned::Spanned, Error, Ident, Type},
    thiserror::Error,
    thunderdome::{Arena, Index},
};

pub mod parse;

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
}

#[derive(Debug, Clone)]
pub struct SpannedCompileError {
    span: Span,
    kind: CompileError,
}

/// A shim for parsing the root level of a macro invocation, so
/// that a session type may be written as a block w/o an extra
/// layer of braces.
#[derive(Debug, Clone)]
pub struct Invocation {
    pub syntax: SyntaxNode,
}

#[derive(Debug, Clone)]
pub struct SyntaxNode {
    pub expr: Syntax,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Syntax {
    Recv(Type),
    Send(Type),
    Call(Box<SyntaxNode>),
    Choose(Vec<SyntaxNode>),
    Offer(Vec<SyntaxNode>),
    Split(Box<SyntaxNode>, Box<SyntaxNode>),
    Loop(Option<String>, Box<SyntaxNode>),
    Break(Option<String>),
    Continue(Option<String>),
    Block(Vec<SyntaxNode>),
    Type(Type),
}

#[derive(Debug, Clone)]
enum Ir {
    Done,
    Recv(Type),
    Send(Type),
    Call(Index),
    Choose(Vec<Index>),
    Offer(Vec<Index>),
    Split(Index, Index),
    Loop(Option<String>, Index),
    IndexedBreak(usize),
    IndexedContinue(usize),
    LabeledBreak(String),
    LabeledContinue(String),
    Type(Type),
    Error,
}

#[derive(Clone, Debug)]
pub enum Target {
    Done,
    Recv(Type, Rc<Target>),
    Send(Type, Rc<Target>),
    Choose(Vec<Target>),
    Offer(Vec<Target>),
    Loop(Rc<Target>),
    Continue(usize),
    Split(Rc<Target>, Rc<Target>),
    Call(Rc<Target>, Rc<Target>),
    Then(Rc<Target>, Rc<Target>),
    Type(Type),
    Error,
}

impl Syntax {
    pub fn recv(ty: &str) -> Self {
        Syntax::Recv(syn::parse_str(ty).unwrap())
    }

    pub fn send(ty: &str) -> Self {
        Syntax::Send(syn::parse_str(ty).unwrap())
    }

    pub fn call(callee: impl Into<SyntaxNode>) -> Self {
        Syntax::Call(Box::new(callee.into()))
    }

    pub fn loop_(label: Option<String>, body: impl Into<SyntaxNode>) -> Self {
        Syntax::Loop(label, Box::new(body.into()))
    }

    pub fn type_(ty: &str) -> Self {
        Syntax::Type(syn::parse_str(ty).unwrap())
    }
}

impl From<Syntax> for SyntaxNode {
    fn from(expr: Syntax) -> Self {
        Self {
            expr,
            span: Span::call_site(),
        }
    }
}

impl SyntaxNode {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn to_session(&self) -> Result<Target, Error> {
        let mut cfg = Cfg::new();
        let head = self.to_cfg(&mut cfg).0;
        cfg.eliminate_breaks_and_labels(head);
        let target = cfg.to_target(head);

        match cfg.drain_errors() {
            Some(errors) => Err(errors),
            None => Ok(target),
        }
    }

    pub fn new(syntax: Syntax) -> Self {
        Self::from(syntax)
    }

    fn to_cfg(&self, cfg: &mut Cfg) -> (Index, Index) {
        use Syntax::*;
        let node = match &self.expr {
            Recv(ty) => cfg.spanned(Ir::Recv(ty.clone()), self.span),
            Send(ty) => cfg.spanned(Ir::Send(ty.clone()), self.span),
            Break(Some(label)) => cfg.spanned(Ir::LabeledBreak(label.clone()), self.span),
            Break(None) => cfg.spanned(Ir::IndexedBreak(0), self.span),
            Continue(Some(label)) => cfg.spanned(Ir::LabeledContinue(label.clone()), self.span),
            Continue(None) => cfg.spanned(Ir::IndexedContinue(0), self.span),
            Type(s) => cfg.spanned(Ir::Type(s.clone()), s.span()),
            Call(callee) => {
                let callee_node = callee.to_cfg(cfg).0;
                cfg.spanned(Ir::Call(callee_node), self.span)
            }
            Split(tx, rx) => {
                let tx_node = tx.to_cfg(cfg).0;
                let rx_node = rx.to_cfg(cfg).0;
                cfg.spanned(Ir::Split(tx_node, rx_node), self.span)
            }
            Choose(choices) => {
                let choice_nodes = choices.iter().map(|choice| choice.to_cfg(cfg).0).collect();
                cfg.spanned(Ir::Choose(choice_nodes), self.span)
            }
            Offer(choices) => {
                let choice_nodes = choices.iter().map(|choice| choice.to_cfg(cfg).0).collect();
                cfg.spanned(Ir::Offer(choice_nodes), self.span)
            }
            Loop(maybe_label, body) => {
                let (head, tail) = body.to_cfg(cfg);

                match cfg[tail].expr {
                    Ir::LabeledBreak(_)
                    | Ir::LabeledContinue(_)
                    | Ir::IndexedBreak(_)
                    | Ir::IndexedContinue(_) => {}
                    _ => {
                        let continue_ = cfg.singleton(Ir::IndexedContinue(0));
                        cfg[tail].next = Some(continue_);
                    }
                }

                cfg.spanned(Ir::Loop(maybe_label.clone(), head), self.span)
            }
            Block(stmts) => {
                let mut next_head = None;
                let mut next_tail = None;
                for stmt in stmts.iter().rev() {
                    let (head, tail) = stmt.to_cfg(cfg);
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

        (node, node)
    }
}

#[derive(Debug, Clone)]
struct Scope {
    label: Option<String>,
    cont: Index,
}

#[derive(Debug, Clone)]
struct CfgNode {
    expr: Ir,
    next: Option<Index>,
    span: Span,
}

impl CfgNode {
    fn singleton(expr: Ir) -> Self {
        Self {
            expr,
            next: None,
            span: Span::call_site(),
        }
    }

    fn spanned(expr: Ir, span: Span) -> Self {
        Self {
            expr,
            next: None,
            span,
        }
    }
}

/// A soup of CFG nodes acting as a context for a single compilation unit.
#[derive(Debug)]
struct Cfg {
    arena: Arena<Result<CfgNode, Index>>,
    errors: Vec<SpannedCompileError>,
}

impl ops::Index<Index> for Cfg {
    type Output = CfgNode;

    fn index(&self, index: Index) -> &Self::Output {
        match &self.arena[index] {
            Ok(node) => node,
            Err(i) => &self[*i],
        }
    }
}

impl ops::IndexMut<Index> for Cfg {
    fn index_mut(&mut self, index: Index) -> &mut Self::Output {
        match self.arena[index] {
            Ok(_) => self.arena[index].as_mut().unwrap(),
            Err(redirected) => &mut self[redirected],
        }
    }
}

impl Default for Cfg {
    fn default() -> Self {
        Cfg::new()
    }
}

impl Cfg {
    fn new() -> Self {
        Self {
            arena: Arena::new(),
            errors: Vec::new(),
        }
    }

    fn insert(&mut self, node: CfgNode) -> Index {
        self.arena.insert(Ok(node))
    }

    /// Cause any access to the `from` index to be redirected to the
    /// `to` index.
    fn redirect(&mut self, from: Index, to: Index) {
        self.arena[from] = Err(to);
    }

    /// Turn a node into an error.
    fn error_at(&mut self, node: Index, kind: CompileError) {
        let spanned_err = SpannedCompileError {
            span: self[node].span,
            kind,
        };
        self.errors.push(spanned_err);
    }

    /// Create a new node containing only this expression with no next-node link.
    fn singleton(&mut self, expr: Ir) -> Index {
        self.insert(CfgNode::singleton(expr))
    }

    /// Create a new node w/ an assigned span containing only this expression with no next-node
    /// link.
    fn spanned(&mut self, expr: Ir, span: Span) -> Index {
        self.insert(CfgNode::spanned(expr, span))
    }

    /// Eliminate `LabeledBreak` and `IndexedBreak` nodes, replacing them w/ redirections to the
    /// nodes they reference and effectively dereferencing the continuations they represent. In
    /// addition, eliminate `LabeledContinuation` nodes, replacing them w/ `IndexedContinuation`
    /// nodes.
    fn eliminate_breaks_and_labels(&mut self, node: Index) {
        let mut env = Vec::new();
        eliminate_inner(self, &mut env, node);
        debug_assert!(
            env.is_empty(),
            "mismatched number of push/pop in `eliminate_breaks_and_labels`"
        );

        fn eliminate_inner(cfg: &mut Cfg, env: &mut Vec<Scope>, node: Index) {
            match &cfg[node].expr {
                Ir::Recv(_) | Ir::Send(_) | Ir::Done | Ir::Type(_) | Ir::Error => {
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, next);
                    }
                }
                Ir::Call(callee) => {
                    let callee = *callee;
                    eliminate_inner(cfg, env, callee);
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, next);
                    }
                }
                Ir::Split(tx_only, rx_only) => {
                    let (tx_only, rx_only) = (*tx_only, *rx_only);
                    eliminate_inner(cfg, env, tx_only);
                    eliminate_inner(cfg, env, rx_only);
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, next);
                    }
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    let choices = choices.clone(); // pacify borrowck so cfg isn't borrowed
                    for choice in choices {
                        eliminate_inner(cfg, env, choice);
                    }

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, next);
                    }
                }
                Ir::Loop(label, body) => {
                    let maybe_next = cfg[node].next;
                    let body = *body; // pacify borrowck, or else it thinks cfg is borrowed

                    let scope = Scope {
                        label: label.clone(),
                        cont: maybe_next
                            .unwrap_or_else(|| cfg.insert(CfgNode::singleton(Ir::Done))),
                    };

                    if scope.label.is_some()
                        && env.iter().any(|ancestor| ancestor.label == scope.label)
                    {
                        let label_name = scope.label.as_ref().unwrap().clone();
                        cfg.error_at(node, CompileError::ShadowedLabel(label_name));
                    }

                    env.push(scope);
                    eliminate_inner(cfg, env, body);
                    env.pop();
                }
                Ir::LabeledContinue(label) => {
                    let labeled_index = env
                        .iter()
                        .rev()
                        .position(|s| s.label.as_ref() == Some(label));

                    if let Some(i) = labeled_index {
                        cfg[node].expr = Ir::IndexedContinue(i);
                    } else {
                        let compile_error = CompileError::UndeclaredLabel(label.clone());
                        cfg.error_at(node, compile_error);

                        // Similar to the break elimination cases, we want to eliminate this even
                        // though we can't figure out the exact continuation of it so that we don't
                        // blow up when lowering to the target language. We can do this by just
                        // replacing it with an `Ir::Error` node.
                        cfg[node].expr = Ir::Error;
                    }
                }
                Ir::LabeledBreak(label) => {
                    let labeled_scope = env.iter().rev().find(|s| s.label.as_ref() == Some(label));

                    if let Some(cont) = labeled_scope.map(|s| s.cont) {
                        cfg.redirect(node, cont);
                    } else {
                        let compile_error = CompileError::UndeclaredLabel(label.clone());
                        cfg.error_at(node, compile_error);

                        // Eliminate the break, but replace it with an error node instead. This way,
                        // the CFG can still get by conversion to a session type without panicking
                        // on an uneliminated break.
                        cfg[node].expr = Ir::Error;
                    }
                }
                Ir::IndexedContinue(debruijn_index) => {
                    // Ordinarily, the only way this would encounter a DeBruijn index of > 0 is if
                    // such an indexed continue is generated by hand, maybe in a test. Otherwise the
                    // only way that an `IndexedContinue` is normally generated is through parsing,
                    // which will only ever generate an indexed continue with an index of zero. In
                    // this case, this error means that the user wrote a `continue` outside of a
                    // loop.
                    //
                    // Another reason this error might be hit is if the eliminate-breaks-and-labels
                    // pass is run more than once on the same term, as we generate non-zero indexed
                    // continues from this pass. By not erroring on seeing a DeBruijn index > 0 we
                    // make this pass idempotent. However, as the architecture of the compiler is
                    // currently, this should never happen.
                    if *debruijn_index >= env.len() {
                        cfg.error_at(node, CompileError::ContinueOutsideLoop);
                    }
                }
                Ir::IndexedBreak(debruijn_index) => {
                    // The same caveats as in the above `IndexedContinue` case also apply to the
                    // `IndexedBreak` variant.
                    if *debruijn_index >= env.len() {
                        cfg.error_at(node, CompileError::BreakOutsideLoop);

                        // See rationale for this same construct in `Ir::LabeledBreak` arm.
                        cfg[node].expr = Ir::Error;
                    } else {
                        let cont = env[env.len() - 1 - *debruijn_index].cont;
                        cfg.redirect(node, cont);
                    }
                }
            }
        }
    }

    fn to_target_inner(&self, parent_cont: Target, node: Index) -> Target {
        let node = &self[node];
        let cont = match node.next {
            Some(i) => self.to_target_inner(parent_cont, i),
            None => parent_cont,
        };

        match &node.expr {
            Ir::Done => Target::Done,
            Ir::Recv(t) => Target::Recv(t.clone(), Rc::new(cont)),
            Ir::Send(t) => Target::Send(t.clone(), Rc::new(cont)),
            Ir::Call(callee) => {
                let callee = self.to_target_inner(Target::Done, *callee);
                Target::Call(Rc::new(callee), Rc::new(cont))
            }
            Ir::Split(tx_only, rx_only) => {
                let (tx_only, rx_only) = (*tx_only, *rx_only);
                let tx_target = self.to_target_inner(cont.clone(), tx_only);
                let rx_target = self.to_target_inner(cont, rx_only);
                Target::Split(Rc::new(tx_target), Rc::new(rx_target))
            }
            Ir::Choose(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(cont.clone(), choice))
                    .collect();
                Target::Choose(targets)
            }
            Ir::Offer(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(cont.clone(), choice))
                    .collect();
                Target::Offer(targets)
            }
            Ir::Loop(_, body) => Target::Loop(Rc::new(self.to_target_inner(cont, *body))),
            Ir::IndexedBreak(_) | Ir::LabeledBreak(_) => {
                panic!("uneliminated break in CFG")
            }
            Ir::LabeledContinue(_) => panic!("uneliminated labeled continue in CFG"),
            Ir::IndexedContinue(i) => {
                debug_assert!(node.next.is_none(), "continue must be the end of a block");
                Target::Continue(*i)
            }
            Ir::Type(t) => {
                // Optimize a little bit by doing the `Then` transform ourselves if the next
                // continuation is a `Done`.
                match cont {
                    Target::Done => Target::Type(t.clone()),
                    _ => Target::Then(Rc::new(Target::Type(t.clone())), Rc::new(cont)),
                }
            }
            Ir::Error => match cont {
                Target::Done => Target::Error,
                _ => Target::Then(Rc::new(Target::Error), Rc::new(cont)),
            },
        }
    }

    fn to_target(&self, node: Index) -> Target {
        self.to_target_inner(Target::Done, node)
    }

    fn drain_errors(&mut self) -> Option<syn::Error> {
        let mut maybe_error = None;
        for reported_error in self.errors.drain(..) {
            let new_error = Error::new(reported_error.span, reported_error.kind.to_string());
            match maybe_error.as_mut() {
                None => maybe_error = Some(new_error),
                Some(accumulated_errors) => accumulated_errors.combine(new_error),
            }
        }
        maybe_error
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
            Split(s, p) => write!(f, "Split<{}, {}>", s, p)?,
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
            Error => write!(f, "!")?,
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
            Split(s, p) => quote! { #c::types::Split<#s, #p> }.to_tokens(tokens),
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
            Error => quote! { ! }.to_tokens(tokens),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn tally_client_expr_direct_subst() {
        let mut soup = Cfg::new();
        let send = soup.send("i64");
        let recv = soup.recv("i64");
        let continue_ = soup.singleton(Ir::IndexedContinue(0));
        soup[send].next = Some(continue_);
        let break_ = soup.singleton(Ir::IndexedBreak(0));
        soup[recv].next = Some(break_);
        let choose_opts = vec![send, recv];
        let choose = soup.singleton(Ir::Choose(choose_opts));
        let client_tally = soup.singleton(Ir::Loop(None, choose));
        let continue1 = soup.singleton(Ir::IndexedContinue(1));
        soup[client_tally].next = Some(continue1);

        let break_ = soup.singleton(Ir::IndexedBreak(0));
        let send = soup.send("Operation");
        soup[send].next = Some(client_tally);
        let choose_opts = vec![break_, send];
        let choose = soup.singleton(Ir::Choose(choose_opts));
        let client = soup.singleton(Ir::Loop(None, choose));

        soup.eliminate_breaks_and_labels(client);
        let s = format!("{}", soup.to_target(client));
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<_1>>)>>>)>>");
    }

    #[test]
    fn tally_client_expr_call() {
        let mut soup = Cfg::new();
        let break_ = soup.singleton(Ir::IndexedBreak(0));
        let send = soup.send("Operation");
        let callee = soup.type_("ClientTally");
        let call = soup.singleton(Ir::Call(callee));
        soup[send].next = Some(call);
        let continue_ = soup.singleton(Ir::IndexedContinue(0));
        soup[call].next = Some(continue_);
        let choose_opts = vec![break_, send];
        let choose = soup.singleton(Ir::Choose(choose_opts));
        let client = soup.singleton(Ir::Loop(None, choose));

        soup.eliminate_breaks_and_labels(client);
        let s = format!("{}", soup.to_target(client));
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_expr_call_ast() {
        let client_ast: SyntaxNode = Syntax::Loop(
            None,
            Box::new(
                Syntax::Choose(vec![
                    Syntax::Break(None).into(),
                    Syntax::Block(vec![
                        Syntax::send("Operation").into(),
                        Syntax::call(Syntax::type_("ClientTally")).into(),
                    ])
                    .into(),
                ])
                .into(),
            ),
        )
        .into();

        let s = format!("{}", client_ast.to_session().unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_expr_call_parse_string() {
        let to_parse = "loop {
            choose {
                _0 => break,
                _1 => {
                    send Operation;
                    call ClientTally;
                },
            }
        }";

        let ast = syn::parse_str::<SyntaxNode>(to_parse).unwrap();
        let s = format!("{}", ast.to_session().unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_invocation_call_parse_string() {
        let to_parse = "loop {
                choose {
                    _0 => break,
                    _1 => {
                        send Operation;
                        call ClientTally;
                    },
                }
            }";

        let ast = syn::parse_str::<SyntaxNode>(to_parse).unwrap();
        let s = format!("{}", ast.to_session().unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_invocation_direct_subst_parse_string() {
        let to_parse = "'client: loop {
                choose {
                    _0 => break,
                    _1 => {
                        send Operation;
                        ClientTally;
                    },
                }
            }";

        let ast = syn::parse_str::<SyntaxNode>(to_parse).unwrap();
        let s = format!("{}", ast.to_session().unwrap());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, <ClientTally as Then<Continue>>::Combined>)>>"
        );
    }

    #[test]
    fn hello_invocation() {
        let to_parse = "
                send String;
                recv String;
            ";

        let ast = syn::parse_str::<Invocation>(to_parse).unwrap().syntax;
        let s = format!("{}", ast.to_session().unwrap());
        assert_eq!(s, "Send<String, Recv<String, Done>>");
    }

    #[test]
    fn hello_invocation_double_block() {
        let to_parse = "{
                send String;
                recv String;
            }";

        let ast = syn::parse_str::<Invocation>(to_parse).unwrap().syntax;
        let s = format!("{}", ast.to_session().unwrap());
        assert_eq!(s, "Send<String, Recv<String, Done>>");
    }

    #[test]
    fn tally_client_direct_subst_nested_loop_break() {
        let to_parse = "'client: loop {
            choose {
                _0 => break,
                _1 => {
                    send Operation;
                    loop {
                        choose {
                            _0 => send i64,
                            _1 => {
                                recv i64;
                                continue 'client;
                            }
                        }
                    }
                },
            }
        }";

        let lhs: Type = syn::parse2(
            syn::parse_str::<Invocation>(to_parse)
                .unwrap()
                .syntax
                .to_session()
                .unwrap()
                .into_token_stream(),
        )
        .unwrap();

        let rhs: Type = syn::parse_str(
            "dialectic::types::Loop<
                dialectic::types::Choose<(
                    dialectic::types::Done,
                    dialectic::types::Send<
                        Operation,
                        dialectic::types::Loop<
                            dialectic::types::Choose<(
                                dialectic::types::Send<i64, dialectic::types::Continue>,
                                dialectic::types::Recv<i64, dialectic::types::Continue<dialectic::types::S<dialectic::types::Z>>>,
                            )>
                        >
                    >,
                )>
            >",
        )
        .unwrap();

        assert_eq!(
            lhs.to_token_stream().to_string(),
            rhs.to_token_stream().to_string()
        );
    }

    #[test]
    fn basic_split() {
        let to_parse = "split {
                -> send String,
                <- recv String,
            }";

        let ast = syn::parse_str::<Invocation>(to_parse).unwrap().syntax;
        let s = format!("{}", ast.to_session().unwrap());
        assert_eq!(s, "Split<Send<String, Done>, Recv<String, Done>>");
    }

    #[test]
    fn simple_break_outside_of_loop() {
        let to_parse = "break";
        let error = syn::parse_str::<Invocation>(to_parse)
            .unwrap()
            .syntax
            .to_session()
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            CompileError::BreakOutsideLoop.to_string()
        );
    }

    #[test]
    fn simple_continue_outside_of_loop() {
        let to_parse = "continue";
        let error = syn::parse_str::<Invocation>(to_parse)
            .unwrap()
            .syntax
            .to_session()
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            CompileError::ContinueOutsideLoop.to_string()
        );
    }
}
