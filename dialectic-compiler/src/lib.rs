use {
    proc_macro2::TokenStream,
    quote::{quote, ToTokens},
    std::{fmt, ops},
    syn::Type,
    thunderdome::{Arena, Index},
};

pub mod parse;

/// A shim for parsing the root level of a macro invocation, so
/// that a session type may be written as a block w/o an extra
/// layer of braces.
#[derive(Debug, Clone)]
pub struct Invocation {
    pub ast: Ast,
}

#[derive(Debug, Clone)]
pub enum Ast {
    Recv(Type),
    Send(Type),
    Call(Box<Ast>),
    Choose(Vec<Ast>),
    Offer(Vec<Ast>),
    Loop(Option<String>, Box<Ast>),
    Break(Option<String>),
    Continue(Option<String>),
    Block(Vec<Ast>),
    Type(Type),
}

impl Ast {
    pub fn to_session(&self) -> Session {
        let mut soup = Soup::new();
        let cfg = soup.to_cfg(self);
        soup.to_session(cfg.head)
    }

    pub fn recv(ty: &str) -> Self {
        Ast::Recv(syn::parse_str(ty).unwrap())
    }

    pub fn send(ty: &str) -> Self {
        Ast::Send(syn::parse_str(ty).unwrap())
    }

    pub fn call(callee: Ast) -> Self {
        Ast::Call(Box::new(callee))
    }

    pub fn loop_(label: Option<String>, body: Ast) -> Self {
        Ast::Loop(label, Box::new(body))
    }

    pub fn type_(ty: &str) -> Self {
        Ast::Type(syn::parse_str(ty).unwrap())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CfgSlice {
    pub head: Index,
    pub tail: Index,
}

impl CfgSlice {
    pub fn new(unit: Index) -> Self {
        Self {
            head: unit,
            tail: unit,
        }
    }
}

/// A soup of CFG nodes acting as a context for a single compilation unit.
#[derive(Debug)]
pub struct Soup {
    arena: Arena<Result<Node, Index>>,
}

impl ops::Index<Index> for Soup {
    type Output = Node;

    fn index(&self, index: Index) -> &Self::Output {
        match &self.arena[index] {
            Ok(node) => node,
            Err(i) => &self[*i],
        }
    }
}

impl ops::IndexMut<Index> for Soup {
    fn index_mut(&mut self, index: Index) -> &mut Self::Output {
        match self.arena[index] {
            Ok(_) => self.arena[index].as_mut().unwrap(),
            Err(redirected) => &mut self[redirected],
        }
    }
}

impl Soup {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.arena.len()
    }

    pub fn insert(&mut self, node: Node) -> Index {
        self.arena.insert(Ok(node))
    }

    /// Cause any access to the `from` index to be redirected to the
    /// `to` index.
    pub fn redirect(&mut self, from: Index, to: Index) {
        self.arena[from] = Err(to);
    }

    pub fn unit(&mut self, expr: Expr) -> Index {
        self.insert(Node::unit(expr))
    }

    pub fn send(&mut self, ty: &str) -> Index {
        self.unit(Expr::Send(syn::parse_str(ty).unwrap()))
    }

    pub fn recv(&mut self, ty: &str) -> Index {
        self.unit(Expr::Recv(syn::parse_str(ty).unwrap()))
    }

    pub fn type_(&mut self, ty: &str) -> Index {
        self.unit(Expr::Type(syn::parse_str(ty).unwrap()))
    }

    /// Follow all redirections of an index.
    pub fn find(&self, i: Index) -> Index {
        match self.arena[i] {
            Ok(_) => i,
            Err(r) => self.find(r),
        }
    }

    pub fn find_shorten(&mut self, i: Index) -> Index {
        match self.arena[i] {
            Ok(_) => i,
            Err(r) => {
                let s = self.find_shorten(r);
                self.arena[i] = Err(s);
                s
            }
        }
    }

    pub fn to_cfg(&mut self, ast: &Ast) -> CfgSlice {
        match ast {
            Ast::Recv(ty) => CfgSlice::new(self.unit(Expr::Recv(ty.clone()))),
            Ast::Send(ty) => CfgSlice::new(self.unit(Expr::Send(ty.clone()))),
            Ast::Call(callee) => {
                let callee_node = self.to_cfg(callee).head;
                CfgSlice::new(self.unit(Expr::Call(callee_node)))
            }
            Ast::Choose(choices) => {
                let choice_nodes = choices
                    .iter()
                    .map(|choice| self.to_cfg(choice).head)
                    .collect();
                CfgSlice::new(self.unit(Expr::Choose(choice_nodes)))
            }
            Ast::Offer(choices) => {
                let choice_nodes = choices
                    .iter()
                    .map(|choice| self.to_cfg(choice).head)
                    .collect();
                CfgSlice::new(self.unit(Expr::Offer(choice_nodes)))
            }
            Ast::Loop(maybe_label, body) => {
                let body_cfg = self.to_cfg(body);

                match self[body_cfg.tail].expr {
                    Expr::LabeledBreak(_)
                    | Expr::LabeledContinue(_)
                    | Expr::IndexedBreak(_)
                    | Expr::IndexedContinue(_) => {}
                    _ => {
                        let continue_ = self.unit(Expr::IndexedContinue(0));
                        self[body_cfg.tail].next = Some(continue_);
                    }
                }

                CfgSlice::new(self.unit(Expr::Loop(maybe_label.clone(), body_cfg.head)))
            }
            Ast::Break(Some(label)) => CfgSlice::new(self.unit(Expr::LabeledBreak(label.clone()))),
            Ast::Break(None) => CfgSlice::new(self.unit(Expr::IndexedBreak(0))),
            Ast::Continue(Some(label)) => {
                CfgSlice::new(self.unit(Expr::LabeledContinue(label.clone())))
            }
            Ast::Continue(None) => CfgSlice::new(self.unit(Expr::IndexedContinue(0))),
            Ast::Block(stmts) => {
                let mut next_head = None;
                let mut next_tail = None;
                for stmt in stmts.iter().rev() {
                    let cfg = self.to_cfg(stmt);
                    next_tail = next_tail.or(Some(cfg.tail));
                    self[cfg.tail].next = next_head;
                    next_head = Some(cfg.head);
                }

                let head = next_head.unwrap_or_else(|| self.unit(Expr::Done));
                let tail = next_tail.unwrap_or(head);

                CfgSlice { head, tail }
            }
            Ast::Type(s) => CfgSlice::new(self.unit(Expr::Type(s.clone()))),
        }
    }

    /// Eliminate `LabeledBreak` and `IndexedBreak` nodes, replacing them w/ redirections to the
    /// nodes they reference and effectively dereferencing the continuations they represent. In
    /// addition, eliminate `LabeledContinuation` nodes, replacing them w/ `IndexedContinuation` nodes.
    fn eliminate_breaks_and_labels(&mut self, env: &mut Vec<Scope>, node: Index) {
        match &self[node].expr {
            Expr::Recv(_)
            | Expr::Send(_)
            | Expr::IndexedContinue(_)
            | Expr::Call(_)
            | Expr::Done
            | Expr::Type(_) => {
                if let Some(next) = self[node].next {
                    self.eliminate_breaks_and_labels(env, next);
                }
            }
            Expr::Choose(is) | Expr::Offer(is) => {
                let is = is.clone(); // pacify borrowck so self isn't borrowed
                for i in is {
                    self.eliminate_breaks_and_labels(env, i);
                }
            }
            Expr::Loop(l, i) => {
                let maybe_next = self[node].next;
                let i = *i; // pacify borrowck, or else it thinks self is borrowed

                let scope = Scope {
                    label: l.clone(),
                    cont: maybe_next.unwrap_or_else(|| self.insert(Node::unit(Expr::Done))),
                };

                env.push(scope);
                self.eliminate_breaks_and_labels(env, i);
                env.pop();
            }
            Expr::LabeledContinue(label) => {
                let labeled_index = env
                    .iter()
                    .rev()
                    .position(|s| s.label.as_ref() == Some(label));

                if let Some(i) = labeled_index {
                    self[node].expr = Expr::IndexedContinue(i);
                }
            }
            Expr::LabeledBreak(label) => {
                let labeled_scope = env.iter().rev().find(|s| s.label.as_ref() == Some(label));

                if let Some(cont) = labeled_scope.map(|s| s.cont) {
                    self.redirect(node, cont);
                }
            }
            Expr::IndexedBreak(debruijn_index) => {
                let cont = env[env.len() - 1 - *debruijn_index as usize].cont;
                self.redirect(node, cont);
            }
        }
    }

    fn to_session_inner(&self, node_index: Index, env: &mut Vec<Index>) -> Session {
        let node = &self[node_index];
        match &node.expr {
            Expr::Done => Session::Done,
            Expr::Recv(s) => {
                let cont = node
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| self.to_session_inner(i, env));
                Session::Recv(s.clone(), Box::new(cont.unwrap_or(Session::Done)))
            }
            Expr::Send(s) => {
                let cont = node
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| self.to_session_inner(i, env));
                Session::Send(s.clone(), Box::new(cont.unwrap_or(Session::Done)))
            }
            Expr::Call(s) => {
                let callee = if let Expr::Type(ty) = &self[*s].expr {
                    Session::Type(ty.clone())
                } else {
                    self.to_session_inner(*s, env)
                };

                let cont = node
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| self.to_session_inner(i, env));
                Session::Call(Box::new(callee), Box::new(cont.unwrap_or(Session::Done)))
            }
            Expr::Choose(is) => {
                env.extend(node.next);

                let sessions = is.iter().map(|&i| self.to_session_inner(i, env)).collect();

                if node.next.is_some() {
                    env.pop();
                }

                Session::Choose(sessions)
            }
            Expr::Offer(is) => {
                env.extend(node.next);

                let sessions = is.iter().map(|&i| self.to_session_inner(i, env)).collect();

                if node.next.is_some() {
                    env.pop();
                }

                Session::Offer(sessions)
            }
            Expr::Loop(_, i) => {
                env.extend(node.next);

                let session = self.to_session_inner(*i, env);

                if node.next.is_some() {
                    env.pop();
                }

                Session::Loop(Box::new(session))
            }
            Expr::IndexedBreak(_) | Expr::LabeledBreak(_) => {
                panic!("uneliminated break present in CFG")
            }
            Expr::LabeledContinue(_) => panic!("uneliminated labeled continue present in CFG"),
            Expr::IndexedContinue(i) => {
                assert!(node.next.is_none(), "continue must be the end of a block");
                Session::Continue(*i)
            }
            Expr::Type(s) => {
                let maybe_cont = node
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| self.to_session_inner(i, env));

                match maybe_cont {
                    Some(cont) => Session::Then(Box::new(Session::Type(s.clone())), Box::new(cont)),
                    None => Session::Type(s.clone()),
                }
            }
        }
    }

    pub fn to_session(&mut self, node: Index) -> Session {
        self.eliminate_breaks_and_labels(&mut Vec::new(), node);
        self.to_session_inner(node, &mut Vec::new())
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Done,
    Recv(Type),
    Send(Type),
    Call(Index),
    Choose(Vec<Index>),
    Offer(Vec<Index>),
    Loop(Option<String>, Index),
    IndexedBreak(usize),
    IndexedContinue(usize),
    LabeledBreak(String),
    LabeledContinue(String),
    Type(Type),
}

#[derive(Debug, Clone)]
pub struct Scope {
    label: Option<String>,
    cont: Index,
}

#[derive(Debug, Clone)]
pub struct Node {
    expr: Expr,
    next: Option<Index>,
}

impl Node {
    pub fn unit(expr: Expr) -> Self {
        Self { expr, next: None }
    }
}

#[derive(Clone, Debug)]
pub enum Session {
    Done,
    Recv(Type, Box<Session>),
    Send(Type, Box<Session>),
    Choose(Vec<Session>),
    Offer(Vec<Session>),
    Loop(Box<Session>),
    Continue(usize),
    Split(Box<Session>, Box<Session>),
    Call(Box<Session>, Box<Session>),
    Then(Box<Session>, Box<Session>),
    Type(Type),
}

impl fmt::Display for Session {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Session::*;
        match self {
            Done => write!(f, "Done")?,
            Recv(t, s) => write!(f, "Recv<{}, {}>", t.to_token_stream(), s)?,
            Send(t, s) => write!(f, "Send<{}, {}>", t.to_token_stream(), s)?,
            Loop(s) => write!(f, "Loop<{}>", s)?,
            Split(s, p) => write!(f, "Split<{}, {}>", s, p)?,
            Call(s, p) => write!(f, "Call<{}, {}>", s, p)?,
            Then(s, p) => write!(f, "Then<{}, {}>", s, p)?,
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

impl ToTokens for Session {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use Session::*;
        match self {
            Done => quote! { Done }.to_tokens(tokens),
            Recv(t, s) => quote! { Recv<#t, #s> }.to_tokens(tokens),
            Send(t, s) => quote! { Send<#t, #s> }.to_tokens(tokens),
            Loop(s) => quote! { Loop<#s> }.to_tokens(tokens),
            Split(s, p) => quote! { Split<#s, #p> }.to_tokens(tokens),
            Call(s, p) => quote! { Call<#s, #p> }.to_tokens(tokens),
            Then(s, p) => quote! { Then<#s, #p> }.to_tokens(tokens),
            Choose(cs) => quote! { Choose<(#(#cs,)*)> }.to_tokens(tokens),
            Offer(cs) => quote! { Offer<(#(#cs,)*)> }.to_tokens(tokens),
            Continue(n) => {
                if *n > 0 {
                    (quote! { Continue< }).to_tokens(tokens);
                    (0..*n).for_each(|_| (quote! { S< }).to_tokens(tokens));
                    (quote! { Z }).to_tokens(tokens);
                    (0..=*n).for_each(|_| (quote! { > }).to_tokens(tokens));
                } else {
                    (quote! { Continue }).to_tokens(tokens);
                }
            }
            Type(s) => quote! { #s }.to_tokens(tokens),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tally_client_expr_direct_subst() {
        let mut soup = Soup::new();
        let send = soup.send("i64");
        let recv = soup.recv("i64");
        let continue_ = soup.unit(Expr::IndexedContinue(0));
        soup[send].next = Some(continue_);
        let break_ = soup.unit(Expr::IndexedBreak(0));
        soup[recv].next = Some(break_);
        let choose_opts = vec![send, recv];
        let choose = soup.unit(Expr::Choose(choose_opts));
        let client_tally = soup.unit(Expr::Loop(None, choose));
        let continue1 = soup.unit(Expr::IndexedContinue(1));
        soup[client_tally].next = Some(continue1);

        let break_ = soup.unit(Expr::IndexedBreak(0));
        let send = soup.send("Operation");
        soup[send].next = Some(client_tally);
        let choose_opts = vec![break_, send];
        let choose = soup.unit(Expr::Choose(choose_opts));
        let client = soup.unit(Expr::Loop(None, choose));

        let s = format!("{}", soup.to_session(client));
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<_1>>)>>>)>>");
    }

    #[test]
    fn tally_client_expr_call() {
        let mut soup = Soup::new();
        let break_ = soup.unit(Expr::IndexedBreak(0));
        let send = soup.send("Operation");
        let callee = soup.type_("ClientTally");
        let call = soup.unit(Expr::Call(callee));
        soup[send].next = Some(call);
        let continue_ = soup.unit(Expr::IndexedContinue(0));
        soup[call].next = Some(continue_);
        let choose_opts = vec![break_, send];
        let choose = soup.unit(Expr::Choose(choose_opts));
        let client = soup.unit(Expr::Loop(None, choose));

        let s = format!("{}", soup.to_session(client));
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_expr_call_ast() {
        let client_ast = Ast::loop_(
            None,
            Ast::Choose(vec![
                Ast::Break(None),
                Ast::Block(vec![
                    Ast::send("Operation"),
                    Ast::call(Ast::type_("ClientTally")),
                ]),
            ]),
        );

        let s = format!("{}", client_ast.to_session());
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

        let ast = syn::parse_str::<Ast>(to_parse).unwrap();
        let s = format!("{}", ast.to_session());
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

        let ast = syn::parse_str::<Ast>(to_parse).unwrap();
        let s = format!("{}", ast.to_session());
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

        let ast = syn::parse_str::<Ast>(to_parse).unwrap();
        let s = format!("{}", ast.to_session());
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Then<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn hello_invocation() {
        let to_parse = "
                send String;
                recv String;
            ";

        let ast = syn::parse_str::<Invocation>(to_parse).unwrap().ast;
        let s = format!("{}", ast.to_session());
        assert_eq!(s, "Send<String, Recv<String, Done>>");
    }

    #[test]
    fn hello_invocation_double_block() {
        let to_parse = "{
                send String;
                recv String;
            }";

        let ast = syn::parse_str::<Invocation>(to_parse).unwrap().ast;
        let s = format!("{}", ast.to_session());
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
                .ast
                .to_session()
                .into_token_stream(),
        )
        .unwrap();

        let rhs: Type = syn::parse_str(
            "Loop<
                Choose<(
                    Done,
                    Send<
                        Operation,
                        Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<S<Z>>>,)>>
                    >,
                )>
            >",
        )
        .unwrap();

        assert_eq!(lhs, rhs);
    }
}
