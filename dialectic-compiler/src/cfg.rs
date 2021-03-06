//! The control flow graph representation of a session type.

use {
    proc_macro2::Span,
    std::{collections::HashSet, ops, rc::Rc},
    syn::{Error, Type},
    thunderdome::{Arena, Index},
};

use crate::{flow::FlowAnalysis, target::Target, CompileError, Spanned};

/// A single entry for a node in the CFG.
#[derive(Debug, Clone)]
pub struct CfgNode {
    /// The variant of this node.
    pub expr: Ir,
    /// The continuation of the node, if present. Absence of a continuation indicates the `Done` or
    /// "halting" continuation.
    pub next: Option<Index>,
    /// The associated syntactic span of this node. If an error is emitted regarding this node, this
    /// span may be used to correctly map it to a location in the surface syntax.
    pub span: Span,
    /// "Machine-generated" nodes are allowed to be unreachable. This is used when automatically
    /// inserting continues when resolving scopes, so that if a continue is inserted which isn't
    /// actually reachable, an error isn't emitted.
    pub allow_unreachable: bool,
    /// Nodes keep a hashset of errors in order to deduplicate any errors which are emitted multiple
    /// times for the same node. Errors are traversed when code is emitted during
    /// [`Cfg::generate_target`].
    pub errors: HashSet<CompileError>,
}

/// The "kind" of a CFG node. CFG nodes have additional data stored in [`CfgNode`] which is always
/// the same types and fields for every node, so we separate into the `Ir` variant and `CfgNode`
/// wrapper/entry struct.
#[derive(Debug, Clone)]
pub enum Ir {
    /// Indicating receiving a value of some type.
    Recv(Type),
    /// Indicating sending a value of some type.
    Send(Type),
    /// `Call` nodes act somewhat like a call/cc, and run their body continuation in the same scope
    /// as they are called all the way to "Done" before running their own continuation.
    Call(Option<Index>),
    /// `Choose` nodes have a list of continuations which supersede their "next" pointer. Before
    /// scope resolution, these continuations may be extended by their "implicit" subsequent
    /// continuation, which is stored in the "next" pointer of the node. The scope resolution pass
    /// "lowers" this next pointer continuation into the arms of the `Choose`, and so after scope
    /// resolution all `Choose` nodes' next pointers should be `None`.
    Choose(Vec<Option<Index>>),
    /// Like `Choose`, `Offer` nodes have a list of choices, and after scope resolution have no
    /// continuation.
    Offer(Vec<Option<Index>>),
    /// `Split` nodes have a transmit-only half and a receive-only half. The nodes' semantics are
    /// similar to `Call`.
    Split {
        /// The transmit-only half.
        tx_only: Option<Index>,
        /// The receive-only half.
        rx_only: Option<Index>,
    },
    /// Early on, loops *may* have a body; however, after scope resolution, all loops should have
    /// their bodies be `Some`. So post scope resolution, this field may be unwrapped.
    Loop(Option<Index>),
    /// `Break` nodes directly reference the loop that they break. They can be considered as a
    /// direct reference to the "next" pointer of the referenced loop node.
    Break(Index),
    /// Like break nodes, `Continue` nodes directly reference the loop that they continue.
    /// Semantically they can be considered a reference to the body of the loop, but as they are a
    /// special construct in the target language, we don't resolve them that way.
    Continue(Index),
    /// A "directly injected" type.
    Type(Type),
    /// Emitted when we need to have a node to put errors on but need to not reason about its
    /// behavior.
    Error,
}

impl CfgNode {
    /// Shorthand for creating a [`CfgNode`] with a bunch of default values for its fields, which
    /// you normally want, as well as a default span.
    fn singleton(expr: Ir) -> Self {
        Self {
            expr,
            next: None,
            span: Span::call_site(),
            allow_unreachable: false,
            errors: HashSet::new(),
        }
    }

    /// Shorthand for creating a [`CfgNode`] similarly to [`CfgNode::singleton`], but with an
    /// associated span.
    fn spanned(expr: Ir, span: Span) -> Self {
        Self {
            expr,
            next: None,
            span,
            allow_unreachable: false,
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
    /// Construct an empty control flow graph.
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    /// Insert a constructed `CfgNode` into the CFG, and get back its newly minted index.
    pub fn insert(&mut self, node: CfgNode) -> Index {
        self.arena.insert(node)
    }

    /// Insert an `Error` node as a shim.
    pub fn insert_error_at(&mut self, node: Index, kind: CompileError) {
        self[node].errors.insert(kind);
    }

    /// Create a dummy `Error` node, as a stand-in for a node which should have been generated but
    /// for some reason cannot be.
    pub fn create_error(&mut self, kind: CompileError, span: Span) -> Index {
        let mut errors = HashSet::new();
        errors.insert(kind);
        self.insert(CfgNode {
            expr: Ir::Error,
            next: None,
            span,
            errors,

            // This node is actually usually generated during parsing for things like a `break` or
            // `continue`. So, we actually do want to treat it as if the user wrote it themselves.
            allow_unreachable: false,
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
    /// that the arms of the choose construct don't have to worry about the continuation that comes
    /// "after" *in a higher/parent scope,* we have the resolve_scopes pass to "lower" this
    /// continuation which implicitly follows every arm of the `Choose`, into becoming the
    /// continuation of every relevant arm of the `Choose`. This does have some special cases, for
    /// example we don't want to change or set a next continuation for a `Break` node or `Continue`
    /// node.
    pub fn resolve_scopes(&mut self, node: Option<Index>) {
        // Depth-first once-only traversal of CFG, skipping nodes we've already seen
        let mut visited = HashSet::new();

        // The stack tracks pairs of implicit continuations (the absence of which indicates the
        // `Done` continuation), and indices which might have that continuation or have children
        // which might have that continuation. We push newly discovered nodes onto this stack, and
        // pop from it to drive the traversal. We initialize the stack with the root node, if it is
        // present.
        let mut stack = vec![];
        stack.extend(node.map(|root| (None, root)));

        while let Some((implicit_cont, node)) = stack.pop() {
            // "Follow" a node by checking to see if we have visited it, and if not, pushing it on
            // to the stack to be visited later along with the supplied scoped implicit
            // continuation.
            let mut follow = |implicit_cont, node| {
                if let Some(node) = node {
                    if visited.insert(node) {
                        stack.push((implicit_cont, node));
                    }
                }
            };

            // This match is followed by a statement which checks for a continuation and adds it to
            // the queue if it finds one. As such, any branch which needs specifically to not check
            // its continuation and recursively resolve scopes inside it, will be ended with a
            // `continue`, to skip this recursion.
            //
            // Specifically this is *very* important for nodes such as `Offer` and `Choose`, which
            // will no longer have a continuation at all after this pass, as their continuations
            // will be "lifted" into each arm of the node.
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
                Ir::Split { tx_only, rx_only } => {
                    // `split` expressions evaluate until done; there is no implicit continuation
                    // this is why we pass the `None` implicit continuation to `follow`
                    follow(None, *tx_only);
                    follow(None, *rx_only);
                }
                Ir::Choose(choices) | Ir::Offer(choices) => {
                    // Inline the current implicit continuation into the next pointer of the node,
                    // if it is `Some`
                    follow(implicit_cont, *next);
                    // *Take* the next pointer out so that it is now None, as we are eliminating the
                    // continuation of this node
                    let new_implicit_cont = next.take().or(implicit_cont);

                    // Follow each arm of the `choose`/`offer` using the new implicit continuation
                    for choice in choices.iter_mut() {
                        // If the choice is `Done`, we need to reassign it to the new implicit
                        // continuation. Otherwise, we follow it.
                        if choice.is_some() {
                            follow(new_implicit_cont, *choice);
                        } else {
                            *choice = new_implicit_cont;
                        }
                    }
                }
                Ir::Loop(body) => {
                    let body = *body;
                    let continue0 = self.singleton(Ir::Continue(node));
                    // We generated this without knowing for sure whether it is reachable or not, so
                    // mark it machine-generated so that it doesn't trigger an unreachable code
                    // error.
                    self[continue0].allow_unreachable = true;
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

            // Reborrow here because the `Loop` clause loses the borrow on self from expr/next.
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
                Ir::Split { tx_only, rx_only } => {
                    stack.extend(tx_only.iter().chain(rx_only));

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

        let mut errors = Vec::new();
        for (node_index, node) in self.iter().filter(|(i, _)| reachable.contains(i)) {
            // The boundary of dead code is: when an *impassable* node has a continuation which
            // itself is *unreachable*.
            if let Some(cont_index) = node.next {
                if !flow.is_passable(node_index)
                    && !self[cont_index].allow_unreachable
                    && !reachable.contains(&cont_index)
                {
                    // Emit an error for the node which causes the unreachability, and the node
                    // which is made to be unreachable.
                    errors.push((node_index, CompileError::FollowingCodeUnreachable));
                    errors.push((cont_index, CompileError::UnreachableStatement));
                }
            }
        }

        // Insert all the discovered errors at once
        for (index, error) in errors {
            self.insert_error_at(index, error);
        }
    }

    /// Traverse a [`Cfg`] rooted at the specified index to produce either a valid [`Target`]
    /// corresponding to it, or a [`syn::Error`] corresponding to one or more compiler errors.
    ///
    /// **Important:** This function **must** be called **after** the scope resolution pass, or else
    /// the resultant code may omit sections of the input control flow graph due to implicit
    /// continuations which have not yet been resolved.
    pub fn generate_target(&mut self, node: Option<Index>) -> Result<Spanned<Target>, Error> {
        // The loop environment is a stack of loop head indices, paired with whether or not they are
        // currently known to be "productive": i.e., whether they contain something other than
        // another loop before they hit their corresponding continue.
        struct LoopEnv {
            stack: Vec<(Index, bool)>,
        }

        impl LoopEnv {
            // Make a new loop environment.
            fn new() -> Self {
                Self { stack: Vec::new() }
            }

            // Push a new loop index (initially tagged un-productive) onto the stack.
            fn push(&mut self, index: Index) {
                self.stack.push((index, false));
            }

            // Mark the most inner loop as being productive, if one exists.
            fn mark_productive(&mut self) {
                if let Some(top) = self.stack.last_mut() {
                    top.1 = true;
                }
            }

            // Check if the loop corresponding to the specified de Bruijn index is currently known
            // to be productive.
            fn productive_for_continue(&self, n: usize) -> bool {
                self.stack.iter().rev().take(n + 1).any(|&(_, p)| p)
            }

            // Compute the de Bruijn index of a loop given its CFG index.
            fn debruijn_index_of(&self, jump_target: Index) -> Option<usize> {
                self.stack
                    .iter()
                    .rev()
                    .position(|&(loop_node, _)| loop_node == jump_target)
            }

            fn pop(&mut self) {
                self.stack.pop();
            }
        }

        fn generate_inner(
            cfg: &Cfg,
            errors: &mut Vec<(Index, CompileError)>,
            loop_env: &mut LoopEnv,
            maybe_node: Option<Index>,
        ) -> Spanned<Target> {
            let (node, node_index) = match maybe_node {
                Some(node) => (&cfg[node], node),
                None => {
                    return Spanned {
                        inner: Target::Done,
                        span: Span::call_site(),
                    }
                }
            };

            // In all cases except for `loop`, `continue`, and `break` nodes, we mark the node's
            // current loop environment as being productive.
            match &node.expr {
                Ir::Loop(_) | Ir::Continue(_) | Ir::Break(_) => {}
                _ => loop_env.mark_productive(),
            }

            let target = match &node.expr {
                Ir::Recv(t) => {
                    let t = t.clone();
                    let cont = generate_inner(cfg, errors, loop_env, node.next);
                    Target::Recv(t, Rc::new(cont))
                }
                Ir::Send(t) => {
                    let cont = generate_inner(cfg, errors, loop_env, node.next);
                    Target::Send(t.clone(), Rc::new(cont))
                }
                Ir::Call(callee) => {
                    let cont = generate_inner(cfg, errors, loop_env, node.next);
                    let callee = generate_inner(cfg, errors, loop_env, *callee);

                    Target::Call(Rc::new(callee), Rc::new(cont))
                }
                Ir::Split { tx_only, rx_only } => {
                    let cont = generate_inner(cfg, errors, loop_env, node.next);
                    let (tx_only, rx_only) = (*tx_only, *rx_only);
                    let tx_target = generate_inner(cfg, errors, loop_env, tx_only);
                    let rx_target = generate_inner(cfg, errors, loop_env, rx_only);

                    Target::Split {
                        tx_only: Rc::new(tx_target),
                        rx_only: Rc::new(rx_target),
                        cont: Rc::new(cont),
                    }
                }
                Ir::Choose(choices) => {
                    let targets = choices
                        .iter()
                        .map(|&choice| generate_inner(cfg, errors, loop_env, choice))
                        .collect();
                    debug_assert!(node.next.is_none(), "non-`Done` continuation for `Choose`");

                    Target::Choose(targets)
                }
                Ir::Offer(choices) => {
                    let targets = choices
                        .iter()
                        .map(|&choice| generate_inner(cfg, errors, loop_env, choice))
                        .collect();
                    debug_assert!(node.next.is_none(), "non-`Done` continuation for `Offer`");

                    Target::Offer(targets)
                }
                Ir::Loop(body) => {
                    loop_env.push(node_index);

                    let target =
                        Target::Loop(Rc::new(generate_inner(cfg, errors, loop_env, *body)));

                    loop_env.pop();

                    target
                }
                Ir::Continue(of_loop) => {
                    let jump_target = *of_loop;
                    let debruijn_index = loop_env
                        .debruijn_index_of(jump_target)
                        .expect("continues are always well-scoped in the CFG");
                    // Check if we are within a productive loop, and generate errors otherwise
                    if !loop_env.productive_for_continue(debruijn_index) {
                        // Emit an error for the unproductive loop
                        errors.push((jump_target, CompileError::UnproductiveLoop));

                        if !node.allow_unreachable {
                            // We don't emit errors for machine-generated `continue`s, because they
                            // would be unilluminating to the user
                            errors.push((node_index, CompileError::UnproductiveContinue));
                        }
                    }
                    Target::Continue(debruijn_index)
                }
                Ir::Type(t) => {
                    // Optimize a little bit by emitting the `Then` trait invocation only when
                    // necessary (when the continuation is not `Done`)
                    match node.next {
                        None => Target::Type(t.clone()),
                        Some(cont_index) => {
                            let cont = generate_inner(cfg, errors, loop_env, Some(cont_index));
                            Target::Then(
                                Rc::new(Spanned {
                                    inner: Target::Type(t.clone()),
                                    span: node.span,
                                }),
                                Rc::new(cont),
                            )
                        }
                    }
                }
                // Early return cases: we inline these recursive calls, but we don't want to change
                // their spans because they are already correct.
                Ir::Break(of_loop) => {
                    return generate_inner(cfg, errors, loop_env, cfg[*of_loop].next);
                }
                Ir::Error => {
                    return generate_inner(cfg, errors, loop_env, node.next);
                }
            };

            Spanned {
                inner: target,
                span: node.span,
            }
        }

        // Generate the target, collecting unproductivity errors if any are discovered
        let mut errors = Vec::new();
        let output = generate_inner(self, &mut errors, &mut LoopEnv::new(), node);

        // Insert all the found errors at once
        for (index, kind) in errors {
            self.insert_error_at(index, kind);
        }

        // Collect all the errors attached to any node in the syntax tree
        let all_errors = self.arena.iter().flat_map(|(_, node)| {
            node.errors.iter().map(move |err| Spanned {
                inner: err.clone(),
                span: node.span,
            })
        });

        // Merge all collected errors into one `syn::Error`, if any were discovered
        let mut maybe_error = None;
        for reported_error in all_errors {
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

    /// Iterate over all nodes in the CFG. The order is not defined.
    pub fn iter(&self) -> impl Iterator<Item = (Index, &CfgNode)> + '_ {
        self.arena.iter()
    }
}
