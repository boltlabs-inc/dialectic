use {
    std::collections::{HashMap, HashSet, VecDeque},
    thunderdome::Index,
};

use crate::cfg::{Cfg, Ir};

#[derive(Debug, Clone)]
pub struct FlowAnalysis {
    pub passable: HashSet<Index>,
    pub haltable: HashSet<Index>,
    pub jumps: HashMap<Index, Index>,
}

impl FlowAnalysis {
    pub fn is_passable(&self, node: Index) -> bool {
        self.passable.contains(&node)
    }
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

impl Constraint {
    fn to_preconditions(&self, solver: &Solver) -> Dnf {
        match self {
            Constraint::Passable(node) => construct_passable_preconditions(solver, *node),
            Constraint::Haltable(node) => construct_haltable_preconditions(solver, *node),
            Constraint::BreakableTo {
                breaks_from,
                breaks_to,
            } => construct_breakable_to_preconditions(solver, *breaks_from, *breaks_to),
        }
    }
}

pub type Conjunction = Vec<Constraint>;

#[derive(Debug, Clone)]
pub struct Dnf(pub Vec<Conjunction>);

impl Dnf {
    pub fn trivially_false() -> Self {
        Dnf(vec![])
    }

    pub fn trivially_true() -> Self {
        Dnf(vec![vec![]])
    }

    pub fn is_trivially_true(&self) -> bool {
        self.0.iter().any(|c| c.is_empty())
    }

    pub fn only_if(conj: Conjunction) -> Self {
        Dnf(vec![conj])
    }
}

#[derive(Debug)]
pub struct Implication {
    pub dnf: Dnf,
    pub consequence: Constraint,
}

#[derive(Debug, Clone, Copy)]
pub enum Validity {
    Trivial,
    Progress,
    NoProgress,
}

#[derive(Debug)]
pub struct Solver<'a> {
    cfg: &'a Cfg,
    jumps: HashMap<Index, Index>,
    passable: HashSet<Index>,
    haltable: HashSet<Index>,
    breakable_to: HashSet<(Index, Index)>,
    progress: usize,
    queue: VecDeque<Implication>,
    queried: HashSet<Constraint>,
}

impl<'a> Solver<'a> {
    pub fn new(cfg: &'a Cfg, root: Index) -> Self {
        let mut this = Self {
            cfg,
            jumps: cfg.jump_table(root),
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

    fn push(&mut self, constraint: Constraint) -> bool {
        if self.queried.insert(constraint) {
            let dnf = constraint.to_preconditions(self);

            if dnf.is_trivially_true() {
                self.insert_fact(constraint);
            } else {
                self.queue.push_back(Implication {
                    dnf,
                    consequence: constraint,
                });
            }

            true
        } else {
            false
        }
    }

    fn pop(&mut self) -> Option<Implication> {
        if self.progress > self.queue.len() {
            None
        } else {
            self.progress += 1;
            self.queue.pop_front()
        }
    }

    fn trivial(&self, constraint: &Constraint) -> bool {
        match constraint {
            Constraint::Passable(node) => self.passable.contains(node),
            Constraint::Haltable(node) => self.haltable.contains(node),
            Constraint::BreakableTo {
                breaks_from,
                breaks_to,
            } => self.breakable_to.contains(&(*breaks_from, *breaks_to)),
        }
    }

    fn holds(&mut self, dnf: &Dnf) -> Validity {
        if dnf.0.iter().any(|cj| cj.iter().all(|c| self.trivial(c))) {
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

    pub fn solve(mut self) -> FlowAnalysis {
        while let Some(implication) = self.pop() {
            let validity = self.holds(&implication.dnf);

            if let Validity::Trivial = validity {
                self.insert_fact(implication.consequence);
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
            jumps: self.jumps,
        }
    }
}

fn construct_passable_preconditions(solver: &Solver, node: Index) -> Dnf {
    let cfg = solver.cfg;
    match &cfg[node].expr {
        Ir::Send(_) | Ir::Recv(_) | Ir::Type(_) | Ir::Call(None) => Dnf::trivially_true(),
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

        // Control flow through an error node is undefined, so we assume for now
        // that it is trivially passable.
        //
        // FIXME(sleffy): do we want to add a constraint for passability of the
        // child node that the error is wrapping, if there is one?
        Ir::Error(..) => Dnf::trivially_true(),
    }
}

fn construct_haltable_preconditions(solver: &Solver, node_index: Index) -> Dnf {
    let cfg = solver.cfg;
    let node = &cfg[node_index];
    match &node.expr {
        Ir::Send(_) | Ir::Recv(_) | Ir::Type(_) => match node.next {
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

        Ir::Continue(_) => Dnf::only_if(vec![Constraint::Haltable(solver.jumps[&node_index])]),
        Ir::Break(_) => match solver.cfg[solver.jumps[&node_index]].next {
            Some(cont) => Dnf::only_if(vec![Constraint::Haltable(cont)]),
            None => Dnf::trivially_true(),
        },

        // Control flow through an error node is undefined, so for now we ask
        // whether its child is passable and whether its next node is haltable.
        Ir::Error(_, child) => Dnf::only_if(
            child
                .map(Constraint::Passable)
                .into_iter()
                .chain(node.next.map(Constraint::Haltable))
                .collect(),
        ),
    }
}

fn construct_breakable_to_preconditions(
    solver: &Solver,
    breaks_from: Index,
    breaks_to: Index,
) -> Dnf {
    let cfg = solver.cfg;
    let node = &cfg[breaks_from];
    match &node.expr {
        Ir::Send(_) | Ir::Recv(_) | Ir::Type(_) => match node.next {
            Some(cont) => Dnf::only_if(vec![Constraint::BreakableTo {
                breaks_from: cont,
                breaks_to,
            }]),
            None => Dnf::trivially_true(),
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
        Ir::Break(_) if solver.jumps[&breaks_from] == breaks_to => Dnf::trivially_true(),
        Ir::Break(_) | Ir::Continue(_) => Dnf::trivially_false(),

        // Control flow through an error node is undefined, so for now we ask
        // whether its child is BreakableTo the relevant loop *OR* whether its
        // continuation is.
        Ir::Error(_, child) => {
            let mut disj = vec![];

            if let Some(child) = child {
                disj.push(vec![Constraint::BreakableTo {
                    breaks_from: *child,
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
    }
}
