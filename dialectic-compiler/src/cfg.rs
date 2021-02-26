use {
    proc_macro2::Span,
    std::{collections::HashSet, ops, rc::Rc},
    syn::{Error, Type},
    thunderdome::{Arena, Index},
};

use crate::{
    flow::{FlowAnalysis, Solver},
    target::Target,
    CompileError, Spanned,
};

#[derive(Debug, Clone)]
pub struct CfgNode {
    pub expr: Ir,
    pub next: Option<Index>,
    pub span: Span,
    pub machine_generated: bool,
    pub errors: Vec<CompileError>,
}

#[derive(Debug, Clone)]
pub enum Ir {
    Recv(Type),
    Send(Type),
    Call(Option<Index>),
    Choose(Vec<Option<Index>>),
    Offer(Vec<Option<Index>>),
    Split(Option<Index>, Option<Index>),
    Loop(Option<Index>),
    Break(Index),
    Continue(Index),
    Type(Type),
    Error,
}

impl CfgNode {
    fn singleton(expr: Ir) -> Self {
        Self {
            expr,
            next: None,
            span: Span::call_site(),
            machine_generated: false,
            errors: Vec::new(),
        }
    }

    fn spanned(expr: Ir, span: Span) -> Self {
        Self {
            expr,
            next: None,
            span,
            machine_generated: false,
            errors: Vec::new(),
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
        self[node].errors.push(kind);
    }

    /// Create a dummy `Error` node.
    pub fn create_error(&mut self, kind: CompileError, span: Span) -> Index {
        self.insert(CfgNode {
            expr: Ir::Error,
            next: None,
            span,
            machine_generated: false,
            errors: vec![kind],
        })
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

    /// The purpose of the scope resolution pass is to transform "implicit" continuations into
    /// "explicit" continuations. For example, consider the session type `choose { ... }; send ()`.
    /// In this type, the first parsing of the block will result in a `Choose` node with its
    /// continuation/"next" pointer set to point to a `Send(())` node. However, in order to ensure
    /// that the arms of the choose construct don't have to worry about the continuation that
    /// comes "after" *in a higher/parent scope,* we have the resolve_scopes pass to "lift" this
    /// continuation which implicitly follows every arm of the `Choose`, into becoming the
    /// continuation of every relevant arm of the `Choose`. This does have some special cases,
    /// for example we don't want to change or set a next continuation for a `Break` node or
    /// `Continuation` node
    pub fn resolve_scopes(&mut self, node: Option<Index>) {
        let mut visited = HashSet::new();
        let mut queue = node
            .into_iter()
            .map(|node| (None, node))
            .collect::<Vec<(Option<Index>, Index)>>();

        while let Some((scope, node)) = queue.pop() {
            let CfgNode { expr, next, .. } = &mut self[node];

            match expr {
                Ir::Recv(_) | Ir::Send(_) | Ir::Type(_) | Ir::Error => {}
                Ir::Break(_) | Ir::Continue(_) => continue,
                Ir::Call(child) => {
                    if let Some(child) = *child {
                        if visited.insert(child) {
                            queue.push((None, child));
                        }
                    }
                }
                Ir::Split(tx_only, rx_only) => {
                    if let Some(tx_only) = *tx_only {
                        if visited.insert(tx_only) {
                            queue.push((None, tx_only));
                        }
                    }

                    if let Some(rx_only) = *rx_only {
                        if visited.insert(rx_only) {
                            queue.push((None, rx_only));
                        }
                    }
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    let cont = next.or(scope);
                    for &choice in choices.iter().filter_map(Option::as_ref) {
                        if visited.insert(choice) {
                            queue.push((cont, choice));
                        }
                    }

                    // We do not want to resolve the `next` of the `Choose` to anything,
                    // because we no longer need to "scope" it.
                    continue;
                }
                Ir::Loop(body) => {
                    if let Some(body) = *body {
                        let continue0 = self.singleton(Ir::Continue(node));
                        self[continue0].machine_generated = true;
                        if visited.insert(body) {
                            queue.push((Some(continue0), body));
                        }
                    } else {
                        // duplicated due to borrowck errors
                        let continue0 = self.singleton(Ir::Continue(node));
                        self[continue0].machine_generated = true;
                        // reborrow here because we've lost the borrow on `body`
                        self[node].expr = Ir::Loop(Some(continue0));
                    }
                }
            }

            match &mut self[node].next {
                Some(next) => {
                    if visited.insert(*next) {
                        queue.push((scope, *next));
                    }
                }
                next @ None => *next = scope,
            }
        }
    }

    pub fn analyze_control_flow(&self) -> FlowAnalysis {
        Solver::new(self).solve()
    }

    pub fn report_dead_code(&mut self, node: Option<Index>) {
        let root = match node {
            Some(node) => node,
            None => return,
        };

        let flow = self.analyze_control_flow();
        let mut stack = vec![root];
        let mut visited = HashSet::new();

        while let Some(node) = stack.pop() {
            if !visited.insert(node) {
                break;
            }

            match &self[node].expr {
                Ir::Recv(_)
                | Ir::Send(_)
                | Ir::Type(_)
                | Ir::Continue(_)
                | Ir::Break(_)
                | Ir::Error => {}
                Ir::Loop(child) | Ir::Call(child) => stack.extend(*child),
                Ir::Split(tx, rx) => stack.extend(tx.iter().chain(rx.iter())),
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    stack.extend(choices.iter().filter_map(Option::as_ref))
                }
            }

            if let Some(cont) = self[node].next {
                if flow.is_passable(node) {
                    stack.push(cont);
                } else if !self[cont].machine_generated {
                    self.insert_error_at(node, CompileError::FollowingCodeUnreachable);
                    self.insert_error_at(cont, CompileError::UnreachableStatement);
                }
            }
        }
    }

    fn to_target_inner(
        &self,
        errors: &mut Vec<Spanned<CompileError>>,
        loop_base: usize,
        loop_env: &mut Vec<Index>,
        maybe_node: Option<Index>,
    ) -> Target {
        let (node, node_index) = match maybe_node {
            Some(node) => (&self[node], node),
            None => return Target::Done,
        };

        errors.extend(node.errors.iter().map(|err| Spanned {
            inner: err.clone(),
            span: node.span,
        }));

        match &node.expr {
            Ir::Recv(t) => {
                let cont = self.to_target_inner(errors, loop_base, loop_env, node.next);
                Target::Recv(t.clone(), Rc::new(cont))
            }
            Ir::Send(t) => {
                let cont = self.to_target_inner(errors, loop_base, loop_env, node.next);
                Target::Send(t.clone(), Rc::new(cont))
            }
            Ir::Call(callee) => {
                let cont = self.to_target_inner(errors, loop_base, loop_env, node.next);
                let callee = self.to_target_inner(errors, loop_base, loop_env, *callee);

                Target::Call(Rc::new(callee), Rc::new(cont))
            }
            Ir::Split(tx_only, rx_only) => {
                let cont = self.to_target_inner(errors, loop_base, loop_env, node.next);
                let (tx_only, rx_only) = (*tx_only, *rx_only);
                let tx_target = self.to_target_inner(errors, loop_base, loop_env, tx_only);
                let rx_target = self.to_target_inner(errors, loop_base, loop_env, rx_only);

                Target::Split(Rc::new(tx_target), Rc::new(rx_target), Rc::new(cont))
            }
            Ir::Choose(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(errors, loop_base, loop_env, choice))
                    .collect();

                Target::Choose(targets)
            }
            Ir::Offer(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(errors, loop_base, loop_env, choice))
                    .collect();

                Target::Offer(targets)
            }
            Ir::Loop(body) => {
                loop_env.push(node_index);

                let target = Target::Loop(Rc::new(
                    self.to_target_inner(errors, loop_base, loop_env, *body),
                ));

                loop_env.pop();

                target
            }
            Ir::Break(of_loop) => {
                let jump = self[*of_loop].next;
                self.to_target_inner(errors, loop_base + 1, loop_env, jump)
            }
            Ir::Continue(of_loop) => {
                let jump_target = *of_loop;
                Target::Continue(
                    loop_env
                        .iter()
                        .rev()
                        .position(|&loop_node| loop_node == jump_target)
                        .unwrap(),
                )
            }
            Ir::Type(t) => {
                // Optimize a little bit by doing the `Then` transform ourselves if the next
                // continuation is a `Done`.
                match node.next {
                    None => Target::Type(t.clone()),
                    Some(cont_index) => {
                        let cont =
                            self.to_target_inner(errors, loop_base, loop_env, Some(cont_index));
                        Target::Then(Rc::new(Target::Type(t.clone())), Rc::new(cont))
                    }
                }
            }
            Ir::Error => self.to_target_inner(errors, loop_base, loop_env, node.next),
        }
    }

    pub fn to_target(&self, node: Option<Index>) -> Result<Target, Error> {
        let mut errors = Vec::new();
        let output = self.to_target_inner(&mut errors, 0, &mut Vec::new(), node);

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

    pub fn iter(&self) -> impl Iterator<Item = (Index, &CfgNode)> + '_ {
        self.arena.iter()
    }
}
