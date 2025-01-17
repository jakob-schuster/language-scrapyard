use std::{fmt::Display, rc::Rc};

pub mod matcher;

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
    BoolType,
    IntType,
    StrType,

    /// Any type; equal to all, usable anywhere
    Any,
}

impl Ty {
    pub fn equiv(&self, other: &Self) -> bool {
        match (self, other) {
            (Ty::Any, _)
            | (_, Ty::Any)
            | (Ty::BoolType, Ty::BoolType)
            | (Ty::IntType, Ty::IntType)
            | (Ty::StrType, Ty::StrType) => true,
            _ => false,
        }
    }

    pub fn most_precise<'a>(&'a self, other: &'a Self) -> &'a Self {
        match (self, other) {
            (Ty::Any, ty) => ty,
            (ty, Ty::Any) => ty,
            _ => self,
        }
    }
}

pub enum Tm {
    /// Variables just store their index in the environment
    Var {
        index: usize,
    },

    /// Literals
    BoolLit {
        b: bool,
    },
    IntLit {
        i: i32,
    },
    StrLit {
        s: String,
    },

    /// A let binding, just binds one name to one value in the body
    Let {
        n: String,
        head: Rc<Tm>,
        body: Rc<Tm>,
    },

    /// A match expression. Evaluates to the first branch whose pattern matches
    Match {
        tm: Rc<Tm>,
        branches: Vec<Branch>,
    },
}

pub struct Branch {
    /// A function from an environment to a list of new bindings, or a failure to match
    pub matcher: Rc<dyn matcher::Matcher>,

    /// A body to evaluate if the match function succeeds
    pub body: Tm,
}

/// Values
#[derive(Clone, PartialEq, Eq)]
pub enum Vtm {
    Bool { b: bool },
    Int { i: i32 },
    Str { s: String },
}

#[derive(Clone, Default)]
pub struct Env {
    vec: Vec<Vtm>,
}

impl Env {
    fn with(&self, vtm: Vtm) -> Env {
        let mut new_vec = self.vec.clone();
        new_vec.push(vtm);

        Env { vec: new_vec }
    }

    fn get(&self, index: &usize) -> &Vtm {
        match self.vec.get(*index) {
            Some(vtm) => vtm,
            None => panic!("could not index environment"),
        }
    }

    fn len(&self) -> usize {
        self.vec.len()
    }
}
pub fn eval(env: &Env, tm: &Tm) -> Vtm {
    match tm {
        Tm::Var { index } => env.get(index).clone(),
        Tm::BoolLit { b } => Vtm::Bool { b: *b },
        Tm::IntLit { i } => Vtm::Int { i: *i },
        Tm::StrLit { s } => Vtm::Str { s: s.clone() },
        Tm::Let { n, head, body } => {
            let head_val = eval(env, head);

            eval(&env.with(head_val), body)
        }
        Tm::Match { tm, branches } => {
            let vtm = eval(env, tm);

            // go through each branch
            for branch in branches {
                if let Some(vtms) = branch.matcher.evaluate(env, &vtm) {
                    // bind all the values in the env
                    let new_env = vtms
                        .iter()
                        .fold(env.clone(), |env0, vtm| env0.with(vtm.clone()));

                    // evaluate the arm
                    return eval(&new_env, &branch.body);
                }
            }

            panic!("incomplete match?!")
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::BoolType => "Bool".fmt(f),
            Ty::IntType => "Int".fmt(f),
            Ty::StrType => "Str".fmt(f),
            Ty::Any => "Any".fmt(f),
        }
    }
}

impl Display for Vtm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Vtm::Bool { b } => b.fmt(f),
            Vtm::Int { i } => i.fmt(f),
            Vtm::Str { s } => s.fmt(f),
        }
    }
}
