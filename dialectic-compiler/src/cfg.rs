use {
    proc_macro2::Span,
    std::{collections::HashSet, ops, rc::Rc},
    syn::{Error, Type},
    thunderdome::{Arena, Index},
};

use crate::{flow::FlowAnalysis, target::Target, CompileError, Spanned};

#[derive(Debug, Clone)]
pub struct CfgNode {
    pub expr: Ir,
    pub next: Option<Index>,
    pub span: Span,
    pub machine_generated: bool,
    pub errors: HashSet<CompileError>,
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
            errors: HashSet::new(),
        }
    }

    fn spanned(expr: Ir, span: Span) -> Self {
        Self {
            expr,
            next: None,
            span,
            machine_generated: false,
            errors: HashSet::new(),
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
        self[node].errors.insert(kind);
    }

    /// Create a dummy `Error` node, as a stand-in for a node which should have
    /// been generated but for some reason cannot be.
    pub fn create_error(&mut self, kind: CompileError, span: Span) -> Index {
        let mut errors = HashSet::new();
        errors.insert(kind);
        self.insert(CfgNode {
            expr: Ir::Error,
            next: None,
            span,
            errors,

            // This node is actually usually generated during parsing for things
            // like a `break` or `continue`. So, we actually do want to treat it
            // as if the user wrote it themselves.
            machine_generated: false,
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
    /// comes "after" *in a higher/parent scope,* we have the resolve_scopes pass to "lower" this
    /// continuation which implicitly follows every arm of the `Choose`, into becoming the
    /// continuation of every relevant arm of the `Choose`. This does have some special cases,
    /// for example we don't want to change or set a next continuation for a `Break` node or
    /// `Continue` node
    pub fn resolve_scopes(&mut self, node: Option<Index>) {
        // Depth-first once-only traversal of CFG, skipping nodes we've already seen
        let mut visited = HashSet::new();

        // The stack tracks pairs of implicit continuations (the absence of which indicates the
        // `Done` continuation), and indices which might have that continuation or have children
        // which might have that continuation. We push newly discovered nodes onto this stack, and
        // pop from it to drive the traversal.
        let mut stack = node
            .into_iter()
            .map(|node| (None, node))
            .collect::<Vec<(Option<Index>, Index)>>();

        while let Some((implicit_cont, node)) = stack.pop() {
            // "Follow" a node by checking to see if we have visited it, and if not, pushing it on
            // to the stack to be visited later along with the supplied scoped implicit continuation.
            let mut follow = |implicit_cont, node| {
                if let Some(node) = node {
                    if visited.insert(node) {
                        stack.push((implicit_cont, node));
                    }
                }
            };

            // This match is followed by a statement which checks for a continuation
            // and adds it to the queue if it finds one. As such, any branch which needs
            // specifically to not check its continuation and recursively resolve scopes
            // inside it, will be ended with a `continue`, to skip this recursion.
            //
            // Specifically this is *very* important for nodes such as `Offer` and `Choose`,
            // which will no longer have a continuation at all after this pass, as their
            // continuations will be "lifted" into each arm of the node.
            let CfgNode { expr, next, .. } = &mut self[node];
            match expr {
                Ir::Recv(_)
                | Ir::Send(_)
                | Ir::Type(_)
                | Ir::Break(_)
                | Ir::Continue(_)
                | Ir::Error => {}

                Ir::Call(child) => {
                    // `call` expressions evaluate until done; there is no implicit continuation
                    // this is why we pass the `None` implicit continuation to `follow`
                    follow(None, *child);
                }
                Ir::Split(tx_only, rx_only) => {
                    // `split` expressions evaluate until done; there is no implicit continuation
                    // this is why we pass the `None` implicit continuation to `follow`
                    follow(None, *tx_only);
                    follow(None, *rx_only);
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    // *Take* the next pointer out so that it is now None, as we are eliminating
                    // the continuation of this node.
                    let cont = match next.take() {
                        // If we find an explicit continuation, we need to lower it into the arms
                        // of the `Choose` or `Offer`.
                        Some(next) => {
                            follow(implicit_cont, Some(next));
                            Some(next)
                        }
                        // If there is no explicit continuation, then in lieu of replacing the
                        // explicit continuation of the `Choose` and `Offer` node with the scoped
                        // implicit continuation, we want to lower the implicit continuation into
                        // the arms of the `Choose` or `Offer`.
                        None => implicit_cont,
                    };

                    // Follow every arm of the `Choose` / `Offer` using the continuation computed
                    // above.
                    for &choice in choices.iter().filter_map(Option::as_ref) {
                        follow(cont, Some(choice));
                    }
                }
                Ir::Loop(body) => {
                    let body = *body;
                    let continue0 = self.singleton(Ir::Continue(node));
                    self[continue0].machine_generated = true;
                    if let Some(body) = body {
                        // If the loop has a body, process it using the implicit continuation that
                        // continues to the loop itself
                        follow(Some(continue0), Some(body));
                    } else {
                        // Assign the body of the loop to `continue`: this will become an error in a
                        // later pass, because `loop { continue }` is unproductive
                        self[node].expr = Ir::Loop(Some(continue0));
                    }
                }
            }

            // reborrow here because the `Loop` clause loses the borrow on self from expr/next.
            let CfgNode { expr, next, .. } = &mut self[node];
            match next {
                // If the next pointer exists, follow it and continue converting its syntax tree,
                // with the implicit continuation of *our current scope* (i.e. if we're in a `Loop`,
                // we're still in that `Loop` after following the continuation)
                Some(next) => follow(implicit_cont, Some(*next)),
                // If there is no explicit continuation for this node, *and* it is not a `Choose` or
                // `Offer` (which have had their explicit continuations erased and lowered into the
                // arms) then we need to assign the scoped implicit continuation, if there is one,
                // as the new explicit continuation
                None if !matches!(expr, Ir::Choose(_) | Ir::Offer(_)) => *next = implicit_cont,
                // This will only be reached if there is no explicit continuation and the node is a
                // `Choose` or `Offer`, in which case we want to do nothing.
                _ => {}
            }
        }
    }

    /// Compute the set of nodes which are reachable from the root of the [`Cfg`], using the results
    /// of the [`FlowAnalysis`] to determine whether nodes can be passed through.
    fn analyze_reachability(&self, flow: &FlowAnalysis, root: Index) -> HashSet<Index> {
        let mut stack = vec![root];
        let mut reachable = HashSet::new();

        while let Some(node_index) = stack.pop() {
            if !reachable.insert(node_index) {
                continue;
            }

            let node = &self[node_index];

            match &node.expr {
                Ir::Loop(child) | Ir::Call(child) => {
                    stack.extend(child);

                    if flow.is_passable(node_index) {
                        stack.extend(node.next);
                    }
                }
                Ir::Split(tx, rx) => {
                    stack.extend(tx.iter().chain(rx));

                    if flow.is_passable(node_index) {
                        stack.extend(node.next);
                    }
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    stack.extend(choices.iter().filter_map(Option::as_ref));

                    assert!(node.next.is_none(), "at this point in the compiler, all continuations of \
                        `Choose` and `Offer` nodes should have been eliminated by the resolve_scopes pass.");
                }
                Ir::Send(_)
                | Ir::Recv(_)
                | Ir::Type(_)
                | Ir::Break(_)
                | Ir::Continue(_)
                | Ir::Error => {
                    if flow.is_passable(node_index) {
                        stack.extend(node.next);
                    }
                }
            }
        }

        reachable
    }

    /// Attach errors to all dead code and code that leads to dead code, based on an internal call
    /// to the flow analysis and reachability analysis on the graph.
    pub fn report_dead_code(&mut self, node: Option<Index>) {
        let root = match node {
            Some(node) => node,
            None => return,
        };

        let flow = crate::flow::analyze(self);
        let reachable = self.analyze_reachability(&flow, root);
        let mut stack = vec![root];
        let mut visited = HashSet::new();

        while let Some(node_index) = stack.pop() {
            if !visited.insert(node_index) {
                continue;
            }

            let node = &self[node_index];

            match &node.expr {
                Ir::Loop(child) | Ir::Call(child) => stack.extend(*child),
                Ir::Split(tx, rx) => stack.extend(tx.iter().chain(rx.iter())),
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    stack.extend(choices.iter().filter_map(Option::as_ref))
                }
                _ => {}
            }

            if let Some(cont_index) = node.next {
                let cont = &self[cont_index];
                if flow.is_passable(node_index) {
                    stack.push(cont_index);
                } else if !cont.machine_generated && !reachable.contains(&cont_index) {
                    self.insert_error_at(node_index, CompileError::FollowingCodeUnreachable);
                    self.insert_error_at(cont_index, CompileError::UnreachableStatement);
                }
            }
        }
    }

    /// Starting with the empty environment of loops and the empty list of errors, convert a [`Cfg`]
    /// rooted at a specific node into the target language representation, ready for output.
    fn to_target_inner(
        &self,
        errors: &mut Vec<Spanned<CompileError>>,
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
                let cont = self.to_target_inner(errors, loop_env, node.next);
                Target::Recv(t.clone(), Rc::new(cont))
            }
            Ir::Send(t) => {
                let cont = self.to_target_inner(errors, loop_env, node.next);
                Target::Send(t.clone(), Rc::new(cont))
            }
            Ir::Call(callee) => {
                let cont = self.to_target_inner(errors, loop_env, node.next);
                let callee = self.to_target_inner(errors, loop_env, *callee);

                Target::Call(Rc::new(callee), Rc::new(cont))
            }
            Ir::Split(tx_only, rx_only) => {
                let cont = self.to_target_inner(errors, loop_env, node.next);
                let (tx_only, rx_only) = (*tx_only, *rx_only);
                let tx_target = self.to_target_inner(errors, loop_env, tx_only);
                let rx_target = self.to_target_inner(errors, loop_env, rx_only);

                Target::Split(Rc::new(tx_target), Rc::new(rx_target), Rc::new(cont))
            }
            Ir::Choose(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(errors, loop_env, choice))
                    .collect();

                Target::Choose(targets)
            }
            Ir::Offer(choices) => {
                let targets = choices
                    .iter()
                    .map(|&choice| self.to_target_inner(errors, loop_env, choice))
                    .collect();

                Target::Offer(targets)
            }
            Ir::Loop(body) => {
                loop_env.push(node_index);

                let target = Target::Loop(Rc::new(self.to_target_inner(errors, loop_env, *body)));

                loop_env.pop();

                target
            }
            Ir::Break(of_loop) => {
                let jump = self[*of_loop].next;
                self.to_target_inner(errors, loop_env, jump)
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
                        let cont = self.to_target_inner(errors, loop_env, Some(cont_index));
                        Target::Then(Rc::new(Target::Type(t.clone())), Rc::new(cont))
                    }
                }
            }
            Ir::Error => self.to_target_inner(errors, loop_env, node.next),
        }
    }

    /// Traverse a [`Cfg`] rooted at the specified index to produce either a valid [`Target`]
    /// corresponding to it, or a [`syn::Error`] corresponding to one or more compiler errors.
    ///
    /// **Important:** This function **must** be called **after** the scope resolution pass, or else
    /// the resultant code may omit sections of the input control flow graph due to implicit
    /// continuations which have not yet been resolved.
    pub fn to_target(&self, node: Option<Index>) -> Result<Target, Error> {
        let mut errors = Vec::new();
        let output = self.to_target_inner(&mut errors, &mut Vec::new(), node);

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
