use {
    std::collections::{HashSet, VecDeque},
    thunderdome::Index,
};

use crate::cfg::{Cfg, Ir};

#[derive(Debug, Clone)]
pub struct FlowAnalysis {
    pub passable: HashSet<Index>,
    pub haltable: HashSet<Index>,
}

impl FlowAnalysis {
    pub fn is_passable(&self, node: Index) -> bool {
        self.passable.contains(&node)
    }
}

pub fn analyze(cfg: &Cfg) -> FlowAnalysis {
    Solver::new(cfg).solve()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    Passable(Index),
    Haltable(Index),
    BreakableTo {
        breaks_from: Index,
        breaks_to: Index,
    },
}

/// A conjunction of constraints: true if *all* are true.
pub type Conjunction = Vec<Constraint>;

/// A disjunction of conjunctions of constraints: true if *any* of the clauses contain constraints
/// *all of which* are true.
#[derive(Debug, Clone)]
pub struct Dnf(pub Vec<Conjunction>);

impl Dnf {
    /// The trivially false disjunction, unprovable no matter what.
    pub fn trivially_false() -> Self {
        Dnf(vec![])
    }

    /// The trivially true disjunction, provable with no evidence.
    pub fn trivially_true() -> Self {
        Dnf(vec![vec![]])
    }

    /// Tests if a conjunction is *trivially* true (i.e. requires no evidence to be true).
    pub fn is_trivially_true(&self) -> bool {
        self.0.iter().any(|c| c.is_empty())
    }

    /// Construct a disjunction from a single conjunction.
    pub fn only_if(conj: Conjunction) -> Self {
        Dnf(vec![conj])
    }
}

/// An implication in normal form: a set of preconditions which prove a consequence.
///
/// When we can satisfy the preconditions (listed in disjunctive normal form), we are allowed to
/// learn that the consequence is true and insert it into our set of knowledge, which we use to
/// prove further preconditions to be true.
#[derive(Debug)]
pub struct Implication {
    /// The preconditions to the implication.
    pub preconditions: Dnf,
    /// The consequent of the implication.
    pub consequent: Constraint,
}

/// A result from breaking apart a DNF and attempting to determine whether it holds.
#[derive(Debug, Clone, Copy)]
pub enum Validity {
    /// The DNF was proven to be true immediately, based on the knowledge available.
    Trivial,
    /// Progress was made and the solver state was changed by adding new things to be proven.
    Progress,
    /// No progress was made because all the component [`Constraint`]s of the DNF were already in
    /// process of proof, or had been proven.
    NoProgress,
}

/// The current state of the flow analysis solver.
#[derive(Debug)]
pub struct Solver<'a> {
    /// The [`Cfg`] being solved for in this invocation of the analysis.
    cfg: &'a Cfg,
    /// The set of nodes which are currently proven to satisfy the `Passable` constraint.
    passable: HashSet<Index>,
    /// The set of nodes which are currently proven to satisfy the `Haltable` constraint.
    haltable: HashSet<Index>,
    /// The set of pairs of nodes which are currently proven to satisfy the `BreakableTo` constraint, in the order of `breaks_from` followed by `breaks_to`.
    breakable_to: HashSet<(Index, Index)>,
    /// The number of implications examined since the last progress was made. (If this exceeds the length of the implication queue, the solver cannot make any more progress.)
    progress: usize,
    /// The queue of [`Implication`]s yet to be proven.
    queue: VecDeque<Implication>,
    /// The set of all [`Constraint`]s which either have been proven or are in-progress.
    queried: HashSet<Constraint>,
}

impl<'a> Solver<'a> {
    /// Create a new [`Solver`] ready to calculate the flow analysis for the given [`Cfg`].
    pub fn new(cfg: &'a Cfg) -> Self {
        let mut this = Self {
            cfg,
            passable: HashSet::new(),
            haltable: HashSet::new(),
            breakable_to: HashSet::new(),
            progress: 0,
            queue: VecDeque::new(),
            queried: HashSet::new(),
        };

        // Populate initial set of constraints to solve
        for (index, _) in cfg.iter() {
            this.push(Constraint::Passable(index));
        }

        this
    }

    /// Insert a fact into the solver's set of proven knowledge. This should only be called when
    /// this fact has been shown to be true!
    fn insert_fact(&mut self, constraint: Constraint) {
        match constraint {
            Constraint::Passable(node) => self.passable.insert(node),
            Constraint::Haltable(node) => self.haltable.insert(node),
            Constraint::BreakableTo {
                breaks_from,
                breaks_to,
                ..
            } => self.breakable_to.insert((breaks_from, breaks_to)),
        };
    }

    /// Push a constraint into the solver's state, returning whether or not it is currently known to
    /// be true.
    fn push(&mut self, constraint: Constraint) -> bool {
        if self.queried.insert(constraint) {
            let dnf = preconditions(self.cfg, constraint);

            if dnf.is_trivially_true() {
                // Optimization: if the precondition of an implication is trivially true, we don't
                // need to examine its conditions, we can just assert the truth of its consequent
                self.insert_fact(constraint);
            } else {
                self.queue.push_back(Implication {
                    preconditions: dnf,
                    consequent: constraint,
                });
            }

            true
        } else {
            false
        }
    }

    /// Pop a new yet-to-be-proven [`Implication`] from the [`Solver`]'s queue. If no more progress
    /// can be made, or there are no more implications left, returns [`None`].
    fn pop(&mut self) -> Option<Implication> {
        if self.progress > self.queue.len() {
            None
        } else {
            self.progress += 1;
            self.queue.pop_front()
        }
    }

    /// Test if a constraint currently holds, given what we already know.
    fn constraint_holds(&self, constraint: &Constraint) -> bool {
        match constraint {
            Constraint::Passable(node) => self.passable.contains(node),
            Constraint::Haltable(node) => self.haltable.contains(node),
            Constraint::BreakableTo {
                breaks_from,
                breaks_to,
            } => self.breakable_to.contains(&(*breaks_from, *breaks_to)),
        }
    }

    /// Attempt to prove that a DNF holds. If it can not yet be shown to be true, insert its atomic
    /// constraints so they can be proven in the future, potentially.
    fn try_prove(&mut self, dnf: &Dnf) -> Validity {
        if dnf
            .0
            .iter()
            .any(|cj| cj.iter().all(|c| self.constraint_holds(c)))
        {
            Validity::Trivial
        } else {
            let mut progress = false;
            for &constraint in dnf.0.iter().flatten() {
                progress |= self.push(constraint);
            }

            if progress {
                Validity::Progress
            } else {
                Validity::NoProgress
            }
        }
    }

    /// Run the full flow analysis algorithm and return a solution.
    pub fn solve(mut self) -> FlowAnalysis {
        while let Some(implication) = self.pop() {
            let validity = self.try_prove(&implication.preconditions);

            if let Validity::Trivial = validity {
                self.insert_fact(implication.consequent);
            } else {
                self.queue.push_back(implication);
            }

            if let Validity::Trivial | Validity::Progress = validity {
                self.progress = 0;
            }
        }

        FlowAnalysis {
            passable: self.passable,
            haltable: self.haltable,
        }
    }
}

/// Given a [`Constraint`] and the [`Cfg`] to which it pertains, compute the preconditions which
/// would be necessary to satisfy that constraint.
fn preconditions(cfg: &Cfg, constraint: Constraint) -> Dnf {
    return match constraint {
        Constraint::Passable(node) => passable_preconditions(cfg, node),
        Constraint::Haltable(node) => haltable_preconditions(cfg, node),
        Constraint::BreakableTo {
            breaks_from,
            breaks_to,
        } => breakable_to_preconditions(cfg, breaks_from, breaks_to),
    };

    /// Compute the preconditions for a `Passable` constraint.
    fn passable_preconditions(cfg: &Cfg, node: Index) -> Dnf {
        match &cfg[node].expr {
            Ir::Send(_) | Ir::Recv(_) | Ir::Type(_) | Ir::Error | Ir::Call(None) => {
                Dnf::trivially_true()
            }
            Ir::Loop(None) | Ir::Break(_) | Ir::Continue(_) => Dnf::trivially_false(),
            Ir::Call(Some(callee)) => Dnf::only_if(vec![Constraint::Haltable(*callee)]),
            Ir::Split(tx_only, rx_only) => {
                let arms = tx_only.iter().chain(rx_only);
                Dnf::only_if(arms.map(|&arm| Constraint::Haltable(arm)).collect())
            }
            Ir::Offer(choices) | Ir::Choose(choices) => Dnf(choices
                .iter()
                .filter_map(Option::as_ref)
                .map(|&c| vec![Constraint::Passable(c)])
                .collect()),
            Ir::Loop(Some(body)) => Dnf::only_if(vec![Constraint::BreakableTo {
                breaks_from: *body,
                breaks_to: node,
            }]),
        }
    }

    /// Compute the preconditions for a `Haltable` constraint.
    fn haltable_preconditions(cfg: &Cfg, node_index: Index) -> Dnf {
        let node = &cfg[node_index];
        match &node.expr {
            Ir::Send(_) | Ir::Recv(_) | Ir::Type(_) | Ir::Error => match node.next {
                Some(cont) => Dnf::only_if(vec![Constraint::Haltable(cont)]),
                None => Dnf::trivially_true(),
            },
            Ir::Call(_) | Ir::Split(_, _) | Ir::Loop(_) => {
                let mut conj = vec![Constraint::Passable(node_index)];
                conj.extend(node.next.map(Constraint::Haltable));
                Dnf::only_if(conj)
            }
            Ir::Choose(choices) | Ir::Offer(choices) => Dnf(choices
                .iter()
                .filter_map(Option::as_ref)
                .chain(node.next.as_ref())
                .map(|&c| vec![Constraint::Haltable(c)])
                .collect()),

            Ir::Continue(of_loop) => Dnf::only_if(vec![Constraint::Haltable(*of_loop)]),
            Ir::Break(of_loop) => match cfg[*of_loop].next {
                Some(cont) => Dnf::only_if(vec![Constraint::Haltable(cont)]),
                None => Dnf::trivially_true(),
            },
        }
    }

    /// Compute the preconditions for a `BreakableTo` constraint.
    fn breakable_to_preconditions(cfg: &Cfg, breaks_from: Index, breaks_to: Index) -> Dnf {
        let node = &cfg[breaks_from];
        match &node.expr {
            Ir::Send(_) | Ir::Recv(_) | Ir::Type(_) | Ir::Error => match node.next {
                Some(cont) => Dnf::only_if(vec![Constraint::BreakableTo {
                    breaks_from: cont,
                    breaks_to,
                }]),
                None => Dnf::trivially_false(),
            },
            Ir::Call(_) | Ir::Split(_, _) => {
                let mut conj = vec![Constraint::Passable(breaks_from)];
                conj.extend(node.next.map(|cont| Constraint::BreakableTo {
                    breaks_from: cont,
                    breaks_to,
                }));
                Dnf::only_if(conj)
            }
            Ir::Choose(choices) | Ir::Offer(choices) => Dnf(choices
                .iter()
                .filter_map(Option::as_ref)
                .chain(node.next.as_ref())
                .map(|&c| {
                    vec![Constraint::BreakableTo {
                        breaks_from: c,
                        breaks_to,
                    }]
                })
                .collect()),
            Ir::Loop(body) => {
                let mut disj = vec![];

                if let Some(body) = body {
                    disj.push(vec![Constraint::BreakableTo {
                        breaks_from: *body,
                        breaks_to,
                    }]);
                }

                if let Some(cont) = node.next {
                    disj.push(vec![
                        Constraint::Passable(breaks_from),
                        Constraint::BreakableTo {
                            breaks_from: cont,
                            breaks_to,
                        },
                    ]);
                }

                Dnf(disj)
            }
            Ir::Break(of_loop) if *of_loop == breaks_to => Dnf::trivially_true(),
            Ir::Break(_) | Ir::Continue(_) => Dnf::trivially_false(),
        }
    }
}
