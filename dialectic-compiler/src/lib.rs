use {
    std::{
        borrow::Borrow,
        collections::{HashMap, HashSet},
        fmt,
        hash::Hash,
        ops,
    },
    thunderdome::{Arena, Index},
};

pub mod parse;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Modifier {
    Priv,
    Pub,
}

#[derive(Debug, Clone)]
pub struct AstDef {
    modifier: Option<Modifier>,
    lhs: String,
    rhs: Ast,
}

#[derive(Debug, Clone)]
pub enum Ast {
    Recv(String),
    Send(String),
    Call(String),
    Choose(Vec<Ast>),
    Offer(Vec<Ast>),
    Loop(Option<String>, Box<Ast>),
    Break(Option<String>),
    Continue(Option<String>),
    Block(Vec<Ast>),
    Type(String),
}

#[derive(Debug, Clone)]
pub struct Invocation {
    defs: Vec<AstDef>,
}

#[derive(Debug, Clone)]
pub struct SoupDef {
    modifier: Option<Modifier>,
    name: String,
    node: Index,
}

/// A soup of CFG nodes acting as a context for a single compilation unit.
/// Also tracks definitions for direct substitutions.
#[derive(Debug)]
pub struct Soup {
    defs: HashMap<String, SoupDef>,
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

impl<'a, Q: ?Sized> ops::Index<&'a Q> for Soup
where
    String: Borrow<Q>,
    Q: Eq + Hash,
{
    type Output = SoupDef;

    fn index(&self, index: &'a Q) -> &Self::Output {
        &self.defs[index]
    }
}

impl Soup {
    pub fn new() -> Self {
        Self {
            defs: HashMap::new(),
            arena: Arena::new(),
        }
    }

    pub fn count_defs(&self) -> usize {
        self.defs.len()
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

    fn insert_def(&mut self, modifier: Option<Modifier>, name: String, node: Index) {
        self.defs.insert(
            name.clone(),
            SoupDef {
                modifier,
                name,
                node,
            },
        );
    }

    pub fn add_defs(&mut self, invocation: &Invocation) {
        for def in &invocation.defs {
            let node = self.to_cfg(&def.rhs);
            self.insert_def(def.modifier, def.lhs.clone(), node);
        }
    }

    pub fn get_def<'a, Q: ?Sized>(&self, key: &'a Q) -> Option<&SoupDef>
    where
        String: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.defs.get(key)
    }

    pub fn to_cfg(&mut self, ast: &Ast) -> Index {
        match ast {
            Ast::Recv(ty) => self.unit(Expr::Recv(ty.clone())),
            Ast::Send(ty) => self.unit(Expr::Send(ty.clone())),
            Ast::Call(ty) => self.unit(Expr::Call(ty.clone())),
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

    /// Substitute private defs into their reference locations.
    pub fn expand_private_defs(&mut self, visited: &mut HashSet<String>, node: Index) {
        match &self[node].expr {
            Expr::Recv(_)
            | Expr::Send(_)
            | Expr::IndexedBreak(_)
            | Expr::IndexedContinue(_)
            | Expr::LabeledBreak(_)
            | Expr::LabeledContinue(_)
            | Expr::Call(_)
            | Expr::Done => {
                if let Some(next) = self[node].next {
                    self.expand_private_defs(visited, next);
                }
            }
            Expr::Choose(is) | Expr::Offer(is) => {
                let is = is.clone(); // pacify borrowck so self isn't borrowed
                for i in is {
                    self.expand_private_defs(visited, i);
                }
            }
            Expr::Loop(_, i) => {
                let i = *i; // pacify borrowck, or else it thinks self is borrowed
                self.expand_private_defs(visited, i);
            }
            Expr::Type(s) => {
                if let Some(def) = self
                    .defs
                    .get(s)
                    .filter(|def| def.modifier == Some(Modifier::Priv))
                {
                    assert!(
                        visited.insert(s.clone()),
                        "infinite cycle found while expanding private defs"
                    );
                    let def_node = def.node; // Pacify borrowck.
                    self.redirect(node, def_node);
                } else {
                    assert!(
                        self[node].next.is_none(),
                        "cannot substitute raw types anywhere but the end of a block"
                    );
                }
            }
        }
    }

    /// Eliminate `LabeledBreak` and `IndexedBreak` nodes, replacing them w/ redirections to the
    /// nodes they reference and effectively dereferencing the continuations they represent. In
    /// addition, eliminate `LabeledContinuation` nodes, replacing them w/ `IndexedContinuation` nodes.
    pub fn eliminate_breaks_and_labels(&mut self, env: &mut Vec<Scope>, node: Index) {
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

    pub fn perform_all_passes(&mut self) {
        // Pacify borrowck, for now... FIXME(sdleffler) separate arena and defs so we don't need to clone?
        let defs = self.defs.values().cloned().collect::<Vec<SoupDef>>();
        let mut visited = HashSet::new();
        let mut env = Vec::new();
        for def in defs {
            visited.clear();
            visited.insert(def.name.clone());
            self.expand_private_defs(&mut visited, def.node);
            env.clear();
            self.eliminate_breaks_and_labels(&mut env, def.node);
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Done,
    Recv(String),
    Send(String),
    Call(String),
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

    pub fn to_session(&self, arena: &Soup, env: &mut Vec<Index>) -> Session {
        match &self.expr {
            Expr::Done => Session::Done,
            Expr::Recv(s) => {
                let cont = self
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| arena[i].to_session(arena, env));
                Session::Recv(s.clone(), Box::new(cont.unwrap_or(Session::Done)))
            }
            Expr::Send(s) => {
                let cont = self
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| arena[i].to_session(arena, env));
                Session::Send(s.clone(), Box::new(cont.unwrap_or(Session::Done)))
            }
            Expr::Call(s) => {
                let cont = self
                    .next
                    .or_else(|| env.last().cloned())
                    .map(|i| arena[i].to_session(arena, env));
                Session::Call(
                    Box::new(Session::Type(s.clone())),
                    Box::new(cont.unwrap_or(Session::Done)),
                )
            }
            Expr::Choose(is) => {
                env.extend(self.next);

                let sessions = is
                    .iter()
                    .map(|&i| arena[i].to_session(arena, env))
                    .collect();

                if self.next.is_some() {
                    env.pop();
                }

                Session::Choose(sessions)
            }
            Expr::Offer(is) => {
                env.extend(self.next);

                let sessions = is
                    .iter()
                    .map(|&i| arena[i].to_session(arena, env))
                    .collect();

                if self.next.is_some() {
                    env.pop();
                }

                Session::Offer(sessions)
            }
            Expr::Loop(_, i) => {
                env.extend(self.next);

                let session = arena[*i].to_session(arena, env);

                if self.next.is_some() {
                    env.pop();
                }

                Session::Loop(Box::new(session))
            }
            Expr::IndexedBreak(_) | Expr::LabeledBreak(_) => {
                panic!("uneliminated break present in CFG")
            }
            Expr::LabeledContinue(_) => panic!("uneliminated labeled continue present in CFG"),
            Expr::IndexedContinue(i) => {
                assert!(self.next.is_none(), "continue must be the end of a block");
                Session::Continue(*i)
            }
            Expr::Type(s) => {
                assert!(
                    self.next.is_none(),
                    "direct type substitution must be the end of a block"
                );

                if let Some(_) = arena
                    .get_def(s)
                    .filter(|def| def.modifier == Some(Modifier::Priv))
                {
                    panic!("uneliminated private def substitution present in CFG");
                } else {
                    Session::Type(s.clone())
                }
            }
        }
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

        soup.eliminate_breaks_and_labels(&mut Vec::new(), client);

        let s = format!("{}", soup[client].to_session(&soup, &mut Vec::new()));
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<_1>>)>>>)>>");
    }

    #[test]
    fn tally_client_expr_call() {
        let mut soup = Soup::new();
        let break_ = soup.unit(Expr::IndexedBreak(0));
        let send = soup.unit(Expr::Send("Operation".to_owned()));
        let call = soup.unit(Expr::Call("ClientTally".to_owned()));
        soup[send].next = Some(call);
        let continue_ = soup.unit(Expr::IndexedContinue(0));
        soup[call].next = Some(continue_);
        let choose_opts = vec![break_, send];
        let choose = soup.unit(Expr::Choose(choose_opts));
        let client = soup.unit(Expr::Loop(None, choose));

        soup.eliminate_breaks_and_labels(&mut Vec::new(), client);

        let s = format!("{}", soup[client].to_session(&soup, &mut Vec::new()));
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
                    Ast::Call("ClientTally".to_owned()),
                ]),
            ])),
        );

        let mut soup = Soup::new();
        let client_cfg = soup.to_cfg(&client_ast);
        soup.eliminate_breaks_and_labels(&mut Vec::new(), client_cfg);

        let s = format!("{}", soup[client_cfg].to_session(&soup, &mut Vec::new()));
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_expr_call_parse_string() {
        let to_parse = "type Client = loop {
            choose {
                _0 => break,
                _1 => {
                    send(Operation);
                    call(ClientTally);
                },
            }
        };";

        let session_def = syn::parse_str::<AstDef>(to_parse).unwrap();
        let mut soup = Soup::new();
        let client_cfg = soup.to_cfg(&session_def.rhs);
        soup.eliminate_breaks_and_labels(&mut Vec::new(), client_cfg);

        let s = format!("{}", soup[client_cfg].to_session(&soup, &mut Vec::new()));
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_invocation_call_parse_string() {
        let to_parse = "
        type ClientTally =
            loop {
                choose {
                    _0 => send(i64),
                    _1 => {
                        recv(i64);
                        break;
                    },
                }
            };

        type Client =
            loop {
                choose {
                    _0 => break,
                    _1 => {
                        send(Operation);
                        call(ClientTally);
                    },
                }
            };";

        let invocation_ast = syn::parse_str::<Invocation>(to_parse).unwrap();
        let mut soup = Soup::new();
        soup.add_defs(&invocation_ast);
        assert_eq!(soup.count_defs(), 2);
        soup.perform_all_passes();

        let client_tally_cfg = soup["ClientTally"].node;
        let s = format!(
            "{}",
            soup[client_tally_cfg].to_session(&soup, &mut Vec::new())
        );
        assert_eq!(s, "Loop<Choose<(Send<i64, Continue>, Recv<i64, Done>)>>");

        let client_cfg = soup["Client"].node;
        let s = format!("{}", soup[client_cfg].to_session(&soup, &mut Vec::new()));
        assert_eq!(
            s,
            "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
        );
    }

    #[test]
    fn tally_client_invocation_direct_subst_parse_string() {
        let to_parse = "
        priv type ClientTally =
            loop {
                choose {
                    _0 => send(i64),
                    _1 => {
                        recv(i64);
                        continue 'client;
                    },
                }
            };

        type Client =
            'client: loop {
                choose {
                    _0 => break,
                    _1 => {
                        send(Operation);
                        ClientTally
                    },
                }
            };";

        let invocation_ast = syn::parse_str::<Invocation>(to_parse).unwrap();
        let mut soup = Soup::new();
        soup.add_defs(&invocation_ast);
        assert_eq!(soup.count_defs(), 2);
        soup.perform_all_passes();

        let client_cfg = soup["Client"].node;
        let s = format!("{}", soup[client_cfg].to_session(&soup, &mut Vec::new()));
        assert_eq!(s, "Loop<Choose<(Done, Send<Operation, Loop<Choose<(Send<i64, Continue>, Recv<i64, Continue<_1>>)>>>)>>");
    }
}
