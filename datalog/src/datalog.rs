// A naive implementation of Datalog, directly translated from brendan's language garden implementation
// which was itself based on The Essence of Datalog by Mistral Contrastin

// 1. Abstract syntax

use std::{collections::HashMap, fmt::Display};

// Constant terms
#[derive(PartialEq, Hash, Eq, Clone)]
enum Const {
    String(String),
    Int(i32),
}

// Terms that are used in the arguments of atomic symbols
#[derive(Hash, PartialEq, Eq, Clone)]
enum Term {
    Var(String),
    Const(Const),
}

// Atomic symbols. These are considered ground if none of the terms are variables
struct Atom {
    // The predicate symbol of the atom
    name: String,
    // The terms supplied to the atom
    terms: Vec<Term>,
}

// A definite clause
struct Rule {
    // The head or consequent of a rule
    head: Atom,
    // The body or premises of a rule
    body: Vec<Atom>,
}

// The list of known facts about the universe.
// Each atom in this list is assumed to be ground
type KnowledgeBase = Vec<Atom>;

#[derive(Default)]
struct Substitution {
    map: HashMap<Term, Term>,
}

impl Substitution {
    fn lookup(&self, term: &Term) -> Option<&Term> {
        self.map.get(term)
    }

    fn with(&self, t0: Term, t1: Term) -> Substitution {
        let mut map = self.map.clone();
        map.insert(t0, t1);
        Substitution { map }
    }

    fn extend_with(&self, other: &Substitution) -> Substitution {
        let mut map = self.map.clone();
        for (key, val) in other.map.iter() {
            map.insert(key.clone(), val.clone());
        }

        Substitution { map }
    }
}

// A full datalog program, consisting of a list of rules
struct Program {
    rules: Vec<Rule>,
    queries: Vec<Vec<Atom>>,
}

// 2. Pretty printing

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::String(s) => format!("\"{s}\"").fmt(f),
            Const::Int(i) => i.fmt(f),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Var(v) => v.fmt(f),
            Term::Const(c) => c.fmt(f),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &(self.terms)[..] {
            [] => self.name.fmt(f),
            args => format!(
                "{}({})",
                self.name,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
        }
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.body[..] {
            [] => self.head.fmt(f),
            body => format!(
                "{} :- {}",
                self.head,
                body.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
        }
    }
}

// A list of substitutions from variable names to constants
// type Substitution = Vec<(Term, Term)>;

fn substitute(atom: Atom, substitution: &Substitution) -> Atom {
    fn go(term: &Term, substitutition: &Substitution) -> Term {
        match term {
            v @ Term::Var(_) => substitutition
                .lookup(v)
                .expect("Couldn't find term!")
                .clone(),
            c @ Term::Const(_) => c.clone(),
        }
    }

    Atom {
        name: atom.name.clone(),
        terms: atom.terms.iter().map(|t| go(t, substitution)).collect(),
    }
}

// Unification between a body atom and a ground atom
fn unify(atom0: &Atom, atom1: &Atom) -> Option<Substitution> {
    fn go(terms0: &[Term], terms1: &[Term]) -> Option<Substitution> {
        match (terms0, terms1) {
            ([], _) | (_, []) => Some(Substitution::default()),
            ([Term::Const(c0), rest0 @ ..], [Term::Const(c1), rest1 @ ..]) => {
                if c0 == c1 {
                    go(rest0, rest1)
                } else {
                    None
                }
            }
            ([v0 @ Term::Var(_), rest0 @ ..], [c1 @ Term::Const(_), rest1 @ ..]) => {
                match go(rest0, rest1) {
                    Some(subst) => match subst.lookup(v0) {
                        Some(c0) => {
                            if c0 != c1 {
                                None
                            } else {
                                Some(subst.with(v0.clone(), c1.clone()))
                            }
                        }
                        None => Some(subst.with(v0.clone(), c1.clone())),
                    },
                    None => None,
                }
            }
            (_, [v1 @ Term::Var(_), ..]) => panic!("The second atom is assumed to be ground!"),
        }
    }

    if atom0.name == atom1.name {
        go(&atom0.terms, &atom1.terms)
    } else {
        None
    }
}

// Find facts that match the given atom and collect assignments to its
// variables as a list of substitutions.
fn eval_atom(kb: KnowledgeBase, atom: Atom, substs: Vec<Substitution>) -> Vec<Substitution> {
    for subst in substs {
        let downToEarthAtom = substitute(atom, &subst);

        let extension = kb
            .iter()
            .map(|a| unify(&downToEarthAtom, a))
            .collect::<Option<Vec<_>>>()
            .unwrap();

        // subst.extend_with(&extension);
        todo!()
    }

    // subst.extend_with(&extension);
    todo!()
}
