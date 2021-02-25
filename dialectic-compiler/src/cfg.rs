use {
    proc_macro2::Span,
    std::{
        collections::{HashMap, HashSet},
        ops,
        rc::Rc,
    },
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
            let child = mut_node.expr.clone();
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

    pub fn resolve_scopes(&mut self, node: Option<Index>) {
        let mut visited = HashSet::new();
        let mut queue = node
            .into_iter()
            .map(|node| (None, node))
            .collect::<Vec<(Option<Index>, Index)>>();

        while let Some((scope, node)) = queue.pop() {
            let CfgNode { expr, next, .. } = &mut self[node];

            match expr {
                Ir::Recv(_) | Ir::Send(_) | Ir::Type(_) => {}
                Ir::Break(_) | Ir::Continue(_) => continue,
                Ir::Call(child) | Ir::Error(_, child) => {
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
                }
                Ir::Loop(body) => {
                    if let Some(body) = *body {
                        let continue0 = self.singleton(Ir::Continue(0));
                        if visited.insert(body) {
                            queue.push((Some(continue0), body));
                        }
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

    pub fn jump_table(&self, root: Index) -> HashMap<Index, Index> {
        let mut loop_env = Vec::new();
        let mut stack = vec![root];
        let mut jump_table = HashMap::new();

        jump_table_inner(self, &mut jump_table, &mut loop_env, &mut stack);

        return jump_table;

        fn jump_table_inner(
            cfg: &Cfg,
            jump_table: &mut HashMap<Index, Index>,
            loop_env: &mut Vec<Index>,
            stack: &mut Vec<Index>,
        ) {
            while let Some(node_index) = stack.pop() {
                let node = &cfg[node_index];
                match &node.expr {
                    Ir::Recv(_) | Ir::Send(_) | Ir::Type(_) | Ir::Loop(None) => {}
                    Ir::Call(child) | Ir::Error(_, child) => stack.extend(*child),
                    Ir::Split(tx, rx) => stack.extend(tx.iter().chain(rx.iter())),
                    Ir::Choose(choices) | Ir::Offer(choices) => {
                        stack.extend(choices.iter().filter_map(Option::as_ref))
                    }
                    Ir::Break(debruijn_index) | Ir::Continue(debruijn_index) => {
                        let _ = jump_table
                            .insert(node_index, loop_env[loop_env.len() - 1 - *debruijn_index]);
                    }
                    Ir::Loop(Some(body)) => {
                        loop_env.push(node_index);
                        jump_table_inner(cfg, jump_table, loop_env, &mut vec![*body]);
                        loop_env.pop();
                    }
                }

                if let Some(cont) = node.next {
                    stack.push(cont);
                }
            }
        }
    }

    pub fn analyze_control_flow(&self, node: Index) -> FlowAnalysis {
        Solver::new(self, node).solve()
    }

    pub fn report_dead_code(&mut self, node: Option<Index>) {
        let root = match node {
            Some(node) => node,
            None => return,
        };

        let flow = self.analyze_control_flow(root);
        let mut stack = node.into_iter().collect::<Vec<_>>();
        let mut visited = HashSet::new();

        while let Some(node) = stack.pop() {
            if !visited.insert(node) {
                break;
            }

            match &self[node].expr {
                Ir::Recv(_) | Ir::Send(_) | Ir::Type(_) | Ir::Continue(_) | Ir::Break(_) => {}
                Ir::Loop(child) | Ir::Call(child) | Ir::Error(_, child) => stack.extend(*child),
                Ir::Split(tx, rx) => stack.extend(tx.iter().chain(rx.iter())),
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    stack.extend(choices.iter().filter_map(Option::as_ref))
                }
            }

            if let Some(cont) = self[node].next {
                if flow.is_passable(node) {
                    stack.push(cont);
                } else {
                    self.insert_error_at(node, CompileError::FollowingCodeUnreachable);
                    self.insert_error_at(cont, CompileError::UnreachableStatement);
                }
            }
        }
    }

    /// Eliminate `Break` nodes, replacing them w/ redirections to the nodes they reference and
    /// effectively dereferencing the continuations they represent.
    pub fn eliminate_breaks(&mut self, node: Option<Index>) {
        if let Some(node) = node {
            let mut loop_env = Vec::new();
            eliminate_inner(
                self,
                None,
                &mut loop_env,
                |_| unreachable!("all breaks are valid before this pass"),
                node,
            );
            debug_assert!(
                loop_env.is_empty(),
                "mismatched number of push/pop in `eliminate_breaks_and_labels`"
            );
        }

        #[inline]
        fn origin_next(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Option<Index> {
            move |cfg| &mut cfg[node].next
        }

        #[inline]
        fn origin_child(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Option<Index> {
            move |cfg| match &mut cfg[node].expr {
                Ir::Call(child) | Ir::Error(_, child) => child,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_tx_only(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Option<Index> {
            move |cfg| match &mut cfg[node].expr {
                Ir::Split(tx_only, _) => tx_only,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_rx_only(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Option<Index> {
            move |cfg| match &mut cfg[node].expr {
                Ir::Split(_, rx_only) => rx_only,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_choice_n(node: Index, i: usize) -> impl FnOnce(&mut Cfg) -> &mut Option<Index> {
            move |cfg| match &mut cfg[node].expr {
                Ir::Choose(choices) | Ir::Offer(choices) => &mut choices[i],
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        #[inline]
        fn origin_body(node: Index) -> impl FnOnce(&mut Cfg) -> &mut Option<Index> {
            move |cfg| match &mut cfg[node].expr {
                Ir::Loop(body) => body,
                _ => panic!("parent node has unexpectedly mutated"),
            }
        }

        fn eliminate_inner<F>(
            cfg: &mut Cfg,
            next_env: Option<Index>,
            loop_env: &mut Vec<Option<Index>>,
            origin: F,
            node: Index,
        ) where
            F: FnOnce(&mut Cfg) -> &mut Option<Index>,
        {
            match &cfg[node].expr {
                Ir::Recv(_) | Ir::Send(_) | Ir::Type(_) | Ir::Continue(_) => {
                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, next_env, loop_env, origin_next(node), next);
                    }
                }
                Ir::Call(child) | Ir::Error(_, child) => {
                    let child = *child;

                    if let Some(child) = child {
                        eliminate_inner(cfg, next_env, loop_env, origin_child(node), child);
                    }

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, next_env, loop_env, origin_next(node), next);
                    }
                }
                Ir::Split(tx_only, rx_only) => {
                    let (tx_only, rx_only) = (*tx_only, *rx_only);

                    if let Some(tx_only) = tx_only {
                        eliminate_inner(cfg, next_env, loop_env, origin_tx_only(node), tx_only);
                    }

                    if let Some(rx_only) = rx_only {
                        eliminate_inner(cfg, next_env, loop_env, origin_rx_only(node), rx_only);
                    }

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, next_env, loop_env, origin_next(node), next);
                    }
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    // Pacify borrowck by reborrowing
                    let choices = choices.clone();
                    // For each choose/offer arm, eliminate whatever's inside, using that particular
                    // arm's next pointer as the origin.
                    for (i, choice) in choices.into_iter().filter_map(|x| x).enumerate() {
                        eliminate_inner(cfg, next_env, loop_env, origin_choice_n(node, i), choice);
                    }

                    if let Some(next) = cfg[node].next {
                        eliminate_inner(cfg, next_env, loop_env, origin_next(node), next);
                    }
                }
                Ir::Loop(body) => {
                    let maybe_body = *body; // Pacify borrowck, or else it thinks cfg is borrowed
                    let cont = cfg[node].next;

                    if let Some(body) = maybe_body {
                        loop_env.push(cont); // Enter new loop environment
                        eliminate_inner(cfg, next_env, loop_env, origin_body(node), body);
                        let _ = loop_env.pop(); // Exit loop environment
                    }

                    if let Some(cont) = cont {
                        eliminate_inner(cfg, next_env, loop_env, origin_next(node), cont);
                    }
                }
                Ir::Break(index) => {
                    // Pacify borrowck by reborrowing
                    let index = *index;
                    // Set the parent's corresponding next pointer to the continuation referenced by
                    // this `break` statement, stored in the environment
                    *origin(cfg) = loop_env[loop_env.len() - 1 - index];
                }
            }
        }
    }

    fn to_target_inner(
        &self,
        errors: &mut Vec<Spanned<CompileError>>,
        parent_cont: Target,
        maybe_node: Option<Index>,
    ) -> Target {
        let node = match maybe_node {
            Some(node) => &self[node],
            None => return Target::Done,
        };
        let cont = match node.next {
            Some(i) => self.to_target_inner(errors, parent_cont, Some(i)),
            None => parent_cont,
        };

        match &node.expr {
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
                    Some(child) => self.to_target_inner(errors, cont, Some(child)),
                    None => cont,
                }
            }
        }
    }

    pub fn to_target(&self, node: Option<Index>) -> Result<Target, Error> {
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

    pub fn iter(&self) -> impl Iterator<Item = (Index, &CfgNode)> + '_ {
        self.arena.iter()
    }
}
