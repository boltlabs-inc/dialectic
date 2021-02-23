use {
    lazy_static::lazy_static,
    proc_macro2::{Span, TokenStream},
    quote::{quote, ToTokens},
    std::{fmt, mem, ops, rc::Rc},
    syn::{Error, Ident, Type},
    thiserror::Error,
    thunderdome::{Arena, Index},
};

pub mod parse;

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

#[derive(Debug, Clone)]
struct CfgNode {
    expr: Ir,
    next: Option<Index>,
    span: Span,
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
    Loop(Index),
    Break(usize),
    Continue(usize),
    Type(Type),
    Error(CompileError, Option<Index>),
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
    Split(Rc<Target>, Rc<Target>, Rc<Target>),
    Call(Rc<Target>, Rc<Target>),
    Then(Rc<Target>, Rc<Target>),
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
            Type(s) => Ir::Type(s.clone()),
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

/// A cfg of CFG nodes acting as a context for a single compilation unit.
#[derive(Debug)]
struct Cfg {
    arena: Arena<CfgNode>,
}

impl ops::Index<Index> for Cfg {
    type Output = CfgNode;

    fn index(&self, index: Index) -> &Self::Output {
        &self.arena[index]
    }
}

impl ops::IndexMut<Index> for Cfg {
    fn index_mut(&mut self, index: Index) -> &mut Self::Output {
        &mut self.arena[index]
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
        }
    }

    fn insert(&mut self, node: CfgNode) -> Index {
        self.arena.insert(node)
    }

    /// Insert an `Error` node as a shim.
    fn insert_error_at(&mut self, node: Index, kind: CompileError) {
        // Temporarily move the body out of the node.
        let (child, span) = {
            let mut_node = &mut self[node];
            let child = mem::replace(&mut mut_node.expr, Ir::Done);
            let span = mut_node.span;
            (child, span)
        };

        let new_child = self.insert(CfgNode::spanned(child, span));
        self[node].expr = Ir::Error(kind, Some(new_child));
    }

    /// Replace the given node with an error node. This is useful when the node in question is
    /// malformed such that it will cause an assertion failure or additional spurious error further
    /// down the compilation process, and removing it is necessary in order to try and generate any
    /// more errors.
    fn replace_error_at(&mut self, node: Index, kind: CompileError) {
        if let Ir::Error(_, _) = self[node].expr {
            self.insert_error_at(node, kind);
        } else {
            self[node].expr = Ir::Error(kind, None);
        }
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

    /// Eliminate `Break` nodes, replacing them w/ redirections to the nodes they reference and
    /// effectively dereferencing the continuations they represent. In addition, eliminate
    /// `LabeledContinuation` nodes, replacing them w/ `IndexedContinuation` nodes.
    fn eliminate_breaks(&mut self, node: Index) {
        let mut env = Vec::new();
        eliminate_inner(self, &mut env, |_, _| unreachable!(), node);
        debug_assert!(
            env.is_empty(),
            "mismatched number of push/pop in `eliminate_breaks_and_labels`"
        );

        fn assign_next(node: Index) -> impl FnOnce(&mut Cfg, Index) {
            move |cfg, to_assign| cfg[node].next = Some(to_assign)
        }

        fn assign_child(node: Index) -> impl FnOnce(&mut Cfg, Index) {
            move |cfg, to_assign| match &mut cfg[node].expr {
                Ir::Call(child) | Ir::Error(_, Some(child)) => *child = to_assign,
                _ => panic!(),
            }
        }

        fn assign_tx_only(node: Index) -> impl FnOnce(&mut Cfg, Index) {
            move |cfg, to_assign| match &mut cfg[node].expr {
                Ir::Split(tx_only, _) => *tx_only = to_assign,
                _ => panic!(),
            }
        }

        fn assign_rx_only(node: Index) -> impl FnOnce(&mut Cfg, Index) {
            move |cfg, to_assign| match &mut cfg[node].expr {
                Ir::Split(_, rx_only) => *rx_only = to_assign,
                _ => panic!(),
            }
        }

        fn assign_choice_n(node: Index, i: usize) -> impl FnOnce(&mut Cfg, Index) {
            move |cfg, to_assign| match &mut cfg[node].expr {
                Ir::Choose(choices) | Ir::Offer(choices) => choices[i] = to_assign,
                _ => panic!(),
            }
        }

        fn assign_body(node: Index) -> impl FnOnce(&mut Cfg, Index) {
            move |cfg, to_assign| match &mut cfg[node].expr {
                Ir::Loop(body) => *body = to_assign,
                _ => panic!(),
            }
        }

        fn eliminate_inner<F>(cfg: &mut Cfg, env: &mut Vec<Index>, eliminate: F, node: Index)
        where
            F: FnOnce(&mut Cfg, Index),
        {
            match &cfg[node].expr {
                Ir::Done
                | Ir::Recv(_)
                | Ir::Send(_)
                | Ir::Type(_)
                | Ir::Continue(_)
                | Ir::Error(_, None) => {
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, assign_next(node), next);
                    }
                }
                Ir::Call(child) | Ir::Error(_, Some(child)) => {
                    let child = *child;
                    eliminate_inner(cfg, env, assign_child(node), child);
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, assign_next(node), next);
                    }
                }
                Ir::Split(tx_only, rx_only) => {
                    let (tx_only, rx_only) = (*tx_only, *rx_only);
                    eliminate_inner(cfg, env, assign_tx_only(node), tx_only);
                    eliminate_inner(cfg, env, assign_rx_only(node), rx_only);
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, assign_next(node), next);
                    }
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    let choices = choices.clone(); // pacify borrowck so cfg isn't borrowed
                    for (i, choice) in choices.into_iter().enumerate() {
                        eliminate_inner(cfg, env, assign_choice_n(node, i), choice);
                    }

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, assign_next(node), next);
                    }
                }
                Ir::Loop(body) => {
                    let body = *body; // pacify borrowck, or else it thinks cfg is borrowed

                    let maybe_next = cfg[node].next;
                    let cont =
                        maybe_next.unwrap_or_else(|| cfg.insert(CfgNode::singleton(Ir::Done)));

                    env.push(cont);
                    eliminate_inner(cfg, env, assign_body(node), body);
                    env.pop();

                    eliminate_inner(cfg, env, assign_next(node), cont);
                }
                Ir::Break(index) => {
                    let index = *index; // Pacify borrowck.
                    eliminate(cfg, env[env.len() - 1 - index]);
                }
            }
        }
    }

    fn to_target_inner(
        &self,
        errors: &mut Vec<Spanned<CompileError>>,
        parent_cont: Target,
        node: Index,
    ) -> Target {
        let node = &self[node];
        let cont = match node.next {
            Some(i) => self.to_target_inner(errors, parent_cont, i),
            None => parent_cont,
        };

        match &node.expr {
            Ir::Done => Target::Done,
            Ir::Recv(t) => Target::Recv(t.clone(), Rc::new(cont)),
            Ir::Send(t) => Target::Send(t.clone(), Rc::new(cont)),
            Ir::Call(callee) => {
                let callee = self.to_target_inner(errors, Target::Done, *callee);
                Target::Call(Rc::new(callee), Rc::new(cont))
            }
            Ir::Split(tx_only, rx_only) => {
                let (tx_only, rx_only) = (*tx_only, *rx_only);
                let tx_target = self.to_target_inner(errors, Target::Done, tx_only);
                let rx_target = self.to_target_inner(errors, Target::Done, rx_only);
                Target::Split(Rc::new(tx_target), Rc::new(rx_target), Rc::new(cont))
            }
            Ir::Choose(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(errors, cont.clone(), choice))
                    .collect();
                Target::Choose(targets)
            }
            Ir::Offer(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(errors, cont.clone(), choice))
                    .collect();
                Target::Offer(targets)
            }
            Ir::Loop(body) => Target::Loop(Rc::new(self.to_target_inner(errors, cont, *body))),
            Ir::Break(_) => {
                panic!("uneliminated break in CFG")
            }
            Ir::Continue(i) => {
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
            Ir::Error(error, maybe_child) => {
                let spanned_error = Spanned {
                    inner: error.clone(),
                    span: node.span,
                };

                errors.push(spanned_error);

                match *maybe_child {
                    Some(child) => self.to_target_inner(errors, cont, child),
                    None => cont,
                }
            }
        }
    }

    fn to_target(&self, node: Index) -> Result<Target, Error> {
        let mut errors = Vec::new();
        let output = self.to_target_inner(&mut errors, Target::Done, node);

        let mut maybe_error = None;
        for reported_error in errors.drain(..) {
            let new_error = Error::new(reported_error.span, reported_error.inner.to_string());
            match maybe_error.as_mut() {
                None => maybe_error = Some(new_error),
                Some(accumulated_errors) => accumulated_errors.combine(new_error),
            }
        }

        match maybe_error {
            Some(error) => Err(error),
            None => Ok(output),
        }
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
