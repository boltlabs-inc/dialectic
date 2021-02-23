use {
    proc_macro2::Span,
    std::{mem, ops, rc::Rc},
    syn::{Error, Type},
    thunderdome::{Arena, Index},
};

use crate::{target::Target, CompileError, Spanned};

#[derive(Debug, Clone)]
pub struct CfgNode {
    pub expr: Ir,
    pub next: Option<Index>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Ir {
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
pub struct Cfg {
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
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    pub fn insert(&mut self, node: CfgNode) -> Index {
        self.arena.insert(node)
    }

    /// Insert an `Error` node as a shim.
    pub fn insert_error_at(&mut self, node: Index, kind: CompileError) {
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
    pub fn replace_error_at(&mut self, node: Index, kind: CompileError) {
        if let Ir::Error(_, _) = self[node].expr {
            self.insert_error_at(node, kind);
        } else {
            self[node].expr = Ir::Error(kind, None);
        }
    }

    /// Create a new node containing only this expression with no next-node link.
    pub fn singleton(&mut self, expr: Ir) -> Index {
        self.insert(CfgNode::singleton(expr))
    }

    /// Create a new node w/ an assigned span containing only this expression with no next-node
    /// link.
    pub fn spanned(&mut self, expr: Ir, span: Span) -> Index {
        self.insert(CfgNode::spanned(expr, span))
    }

    /// Eliminate `Break` nodes, replacing them w/ redirections to the nodes they reference and
    /// effectively dereferencing the continuations they represent.
    pub fn eliminate_breaks(&mut self, node: Index) {
        let mut env = Vec::new();
        eliminate_inner(
            self,
            &mut env,
            |_| unreachable!("all breaks are valid before this pass"),
            node,
        );
        debug_assert!(
            env.is_empty(),
            "mismatched number of push/pop in `eliminate_breaks_and_labels`"
        );

        #[inline]
        fn origin_next(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Index {
            move |cfg| {
                cfg[node]
                    .next
                    .as_mut()
                    .expect("there will always be a parent")
            }
        }

        #[inline]
        fn origin_child(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Index {
            move |cfg| match &mut cfg[node].expr {
                Ir::Call(child) | Ir::Error(_, Some(child)) => child,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_tx_only(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Index {
            move |cfg| match &mut cfg[node].expr {
                Ir::Split(tx_only, _) => tx_only,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_rx_only(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Index {
            move |cfg| match &mut cfg[node].expr {
                Ir::Split(_, rx_only) => rx_only,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_choice_n(node: Index, i: usize) -> impl FnOnce(&mut Cfg) -> &mut Index {
            move |cfg| match &mut cfg[node].expr {
                Ir::Choose(choices) | Ir::Offer(choices) => &mut choices[i],
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_body(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Index {
            move |cfg| match &mut cfg[node].expr {
                Ir::Loop(body) => body,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        fn eliminate_inner<F>(cfg: &mut Cfg, env: &mut Vec<Index>, origin: F, node: Index)
        where
            F: FnOnce(&mut Cfg) -> &mut Index,
        {
            match &cfg[node].expr {
                Ir::Done
                | Ir::Recv(_)
                | Ir::Send(_)
                | Ir::Type(_)
                | Ir::Continue(_)
                | Ir::Error(_, None) => {
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, origin_next(node), next);
                    }
                }
                Ir::Call(child) | Ir::Error(_, Some(child)) => {
                    let child = *child;
                    eliminate_inner(cfg, env, origin_child(node), child);

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, origin_next(node), next);
                    }
                }
                Ir::Split(tx_only, rx_only) => {
                    let (tx_only, rx_only) = (*tx_only, *rx_only);
                    eliminate_inner(cfg, env, origin_tx_only(node), tx_only);
                    eliminate_inner(cfg, env, origin_rx_only(node), rx_only);

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, origin_next(node), next);
                    }
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    // Pacify borrowck by reborrowing
                    let choices = choices.clone();
                    // For each choose/offer arm, eliminate whatever's inside, using that particular
                    // arm's next pointer as the origin.
                    for (i, choice) in choices.into_iter().enumerate() {
                        eliminate_inner(cfg, env, origin_choice_n(node, i), choice);
                    }

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, env, origin_next(node), next);
                    }
                }
                Ir::Loop(body) => {
                    let body = *body; // Pacify borrowck, or else it thinks cfg is borrowed
                    let cont = cfg[node]
                        .next
                        .unwrap_or_else(|| cfg.insert(CfgNode::singleton(Ir::Done)));

                    env.push(cont); // Enter new loop environment
                    eliminate_inner(cfg, env, origin_body(node), body);
                    let _ = env.pop(); // Exit loop environment

                    eliminate_inner(cfg, env, origin_next(node), cont);
                }
                Ir::Break(index) => {
                    // Pacify borrowck by reborrowing
                    let index = *index;
                    // Set the parent's corresponding next pointer to the continuation referenced by
                    // this `break` statement, stored in the environment
                    *origin(cfg) = env[env.len() - 1 - index];
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
            Ir::Break(_) => panic!("uneliminated break in CFG"),
            Ir::Continue(i) => Target::Continue(*i),
            Ir::Type(t) => {
                // Optimize a little bit by doing the `Then` transform ourselves if the next
                // continuation is a `Done`.
                match cont {
                    Target::Done => Target::Type(t.clone()),
                    _ => Target::Then(Rc::new(Target::Type(t.clone())), Rc::new(cont)),
                }
            }
            Ir::Error(error, maybe_child) => {
                errors.push(Spanned {
                    inner: error.clone(),
                    span: node.span,
                });

                match *maybe_child {
                    Some(child) => self.to_target_inner(errors, cont, child),
                    None => cont,
                }
            }
        }
    }

    pub fn to_target(&self, node: Index) -> Result<Target, Error> {
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
