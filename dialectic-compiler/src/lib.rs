use {
    std::{fmt, ops},
    thunderdome::{Arena, Index},
};

pub mod parse;

#[derive(Debug, Clone)]
pub enum Ast {
    Recv(String),
    Send(String),
    Call(Box<Ast>),
    Choose(Vec<Ast>),
    Offer(Vec<Ast>),
    Loop(Option<String>, Box<Ast>),
    Break(Option<String>),
    Continue(Option<String>),
    Block(Vec<Ast>),
    Type(String),
}

impl Ast {
    pub fn to_session(&self) -> Session {
        let mut soup = Soup::new();
        let cfg = soup.to_cfg(self);
        soup.to_session(cfg)
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

    pub fn to_cfg(&mut self, ast: &Ast) -> Index {
        match ast {
            Ast::Recv(ty) => self.unit(Expr::Recv(ty.clone())),
            Ast::Send(ty) => self.unit(Expr::Send(ty.clone())),
            Ast::Call(callee) => {
                let callee_node = self.to_cfg(callee);
                self.unit(Expr::Call(callee_node))
            }
            Ast::Choose(choices) => {
                let choice_nodes = choices.iter().map(|choice| self.to_cfg(choice)).collect();
                self.unit(Expr::Choose(choice_nodes))
            }
            Ast::Offer(choices) => {
                let choice_nodes = choices.iter().map(|choice| self.to_cfg(choice)).collect();
                self.unit(Expr::Offer(choice_nodes))
            }
            Ast::Loop(maybe_label, body) => {
                let body_node = self.to_cfg(body);

                let mut last_node = body_node;
                loop {
                    if let Some(next) = self[last_node].next {
                        last_node = next;
                    } else {
                        break;
                    }
                }

                match self[last_node].expr {
                    Expr::LabeledBreak(_)
                    | Expr::LabeledContinue(_)
                    | Expr::IndexedBreak(_)
                    | Expr::IndexedContinue(_) => {}
                    _ => {
                        let continue_ = self.unit(Expr::IndexedContinue(0));
                        self[last_node].next = Some(continue_);
                    }
                }

                self.unit(Expr::Loop(maybe_label.clone(), body_node))
            }
            Ast::Break(Some(label)) => self.unit(Expr::LabeledBreak(label.clone())),
            Ast::Break(None) => self.unit(Expr::IndexedBreak(0)),
            Ast::Continue(Some(label)) => self.unit(Expr::LabeledContinue(label.clone())),
            Ast::Continue(None) => self.unit(Expr::IndexedContinue(0)),
            Ast::Block(stmts) => {
                let mut next = None;
                for stmt in stmts.iter().rev() {
                    let node = self.to_cfg(stmt);
                    self[node].next = next;
                    next = Some(node);
                }

                next.unwrap_or_else(|| self.unit(Expr::Done))
            }
            Ast::Type(s) => self.unit(Expr::Type(s.clone())),
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
                    self[node].expr = Expr::IndexedContinue(i as u8);
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
    Recv(String),
    Send(String),
    Call(Index),
    Choose(Vec<Index>),
    Offer(Vec<Index>),
    Loop(Option<String>, Index),
    IndexedBreak(u8),
    IndexedContinue(u8),
    LabeledBreak(String),
    LabeledContinue(String),
    Type(String),
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
    Recv(String, Box<Session>),
    Send(String, Box<Session>),
    Choose(Vec<Session>),
    Offer(Vec<Session>),
    Loop(Box<Session>),
    Continue(u8),
    Split(Box<Session>, Box<Session>),
    Call(Box<Session>, Box<Session>),
    Then(Box<Session>, Box<Session>),
    Type(String),
}

impl fmt::Display for Session {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Session::*;
        match self {
            Done => write!(f, "Done")?,
            Recv(t, s) => write!(f, "Recv<{}, {}>", t, s)?,
            Send(t, s) => write!(f, "Send<{}, {}>", t, s)?,
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
            Type(s) => write!(f, "{}", s)?,
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tally_client_expr_direct_subst() {
        let mut soup = Soup::new();
        let send = soup.unit(Expr::Send("i64".to_owned()));
        let recv = soup.unit(Expr::Recv("i64".to_owned()));
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
        let send = soup.unit(Expr::Send("Operation".to_owned()));
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
        let send = soup.unit(Expr::Send("Operation".to_owned()));
        let callee = soup.unit(Expr::Type("ClientTally".to_owned()));
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
        let client_ast = Ast::Loop(
            None,
            Box::new(Ast::Choose(vec![
                Ast::Break(None),
                Ast::Block(vec![
                    Ast::Send("Operation".to_owned()),
                    Ast::Call(Box::new(Ast::Type("ClientTally".to_owned()))),
                ]),
            ])),
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
        let mut soup = Soup::new();
        let root = soup.to_cfg(&ast);
        soup.eliminate_breaks_and_labels(&mut Vec::new(), root);

        let s = format!("{}", soup.to_session(root));
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
        let mut soup = Soup::new();
        let root = soup.to_cfg(&ast);
        soup.eliminate_breaks_and_labels(&mut Vec::new(), root);

        let s = format!("{}", soup.to_session(root));
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
        let mut soup = Soup::new();
        let root = soup.to_cfg(&ast);
        soup.eliminate_breaks_and_labels(&mut Vec::new(), root);

        let s = format!("{}", soup.to_session(root));
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Then<ClientTally, Continue>>)>>"
        );
    }
}
