use crate::util::{self, Env, Located};

use std::{fmt::Display, rc::Rc};

// De Bruijn index, represents a variable occurrence by the number of
// binders between the occurrence and the binder it refers to.
pub type Index = usize;
// De Bruijn level, represents a variable occurrence by the number of
// binders from the top of the environment to the binder that the
// occurrence refers to. These do not change their meaning as new
// bindings are added to the environment.
pub type Level = usize;
fn level_to_index(size: usize, level: Level) -> Index {
    size - level - 1
}

pub type Tm = Located<TmData>;
/// In dependent typing, types are terms.
pub type Ty = Tm;
#[derive(Clone)]
pub enum TmData {
    /// Let bindings
    Let {
        head: Rc<Tm>,
        body: Rc<Tm>,
    },

    /// Universe of types
    Univ,

    ///
    BoolTy,
    BoolLit {
        b: bool,
    },

    ///
    IntTy,
    IntLit {
        i: i32,
    },

    ///
    StrTy,
    StrLit {
        s: String,
    },

    /// A function type; just contains the types of arguments,
    /// and the return type
    FunTy {
        args: Vec<Ty>,
        body: Rc<Ty>,
    },
    /// A function literal; contains a bunch of arguments
    /// (names have been dropped), a return type, and a function body.
    FunLit {
        args: Vec<Tm>,
        body: Rc<Tm>,
    },
    /// A function application to a bunch of arguments
    FunApp {
        head: Rc<Tm>,
        args: Vec<Tm>,
    },

    /// Variables
    Var {
        index: usize,
    },
}

impl Tm {
    fn is_bound(&self, index: Index) -> bool {
        match &self.data {
            TmData::Let { head, body } => head.is_bound(index) || body.is_bound(index),

            TmData::Univ
            | TmData::BoolTy
            | TmData::BoolLit { b: _ }
            | TmData::IntTy
            | TmData::IntLit { i: _ }
            | TmData::StrTy
            | TmData::StrLit { s: _ } => false,

            TmData::FunTy { args, body: ty } => {
                args.iter().any(|arg| arg.is_bound(index)) || ty.is_bound(index)
            }
            TmData::FunLit { args, body } => {
                // add to the index with the args
                args.iter().any(|arg| arg.is_bound(index)) || body.is_bound(index + args.len())
            }
            TmData::FunApp { head, args } => {
                head.is_bound(index) || args.iter().any(|arg| arg.is_bound(index))
            }

            TmData::Var { index: index1 } => index1.eq(&index),
        }
    }
}

/// Value terms
#[derive(Clone)]
pub enum Vtm {
    // Neutral terms
    Ntm { ntm: Ntm },

    Univ,

    BoolTy,
    Bool { b: bool },

    IntTy,
    Int { i: i32 },

    StrTy,
    Str { s: String },

    FunTy { args: Vec<Vtm>, body: Rc<Vtm> },
    Fun { body: FunData },
}
pub type Vty = Vtm;

impl Vtm {
    pub fn equiv(&self, other: &Vtm) -> bool {
        match (self, other) {
            // trivially equivalent
            (Vtm::Univ, Vtm::Univ)
            | (Vtm::BoolTy, Vtm::BoolTy)
            | (Vtm::IntTy, Vtm::IntTy)
            | (Vtm::StrTy, Vtm::StrTy) => true,

            (
                Vtm::FunTy {
                    args: args1,
                    body: body1,
                },
                Vtm::FunTy {
                    args: args2,
                    body: body2,
                },
            ) => args1.iter().zip(args2).all(|(arg1, arg2)| arg1.equiv(arg2)) && body1.equiv(body2),

            _ => false,
        }
    }
}

/// Function type that explicitly captures its environment.
#[derive(Clone)]
pub struct FunData {
    env: Env<Vtm>,
    body: Tm,
}

impl FunData {
    pub fn app(&self, vtm: &[Vtm]) -> Result<Vtm, EvalError> {
        let new_env = vtm
            .iter()
            .fold(self.env.clone(), |env0, arg| env0.with(arg.clone()));

        eval(&new_env, &self.body)
    }
}

#[derive(Debug)]
pub struct EvalError {
    pub location: util::Location,
    pub message: String,
}

pub fn eval(env: &Env<Vtm>, tm: &Tm) -> Result<Vtm, EvalError> {
    match &tm.data {
        // bind the name and evaluate onwards
        TmData::Let { head, body } => Ok(eval(&env.with(eval(env, head)?), body)?),

        TmData::Univ => Ok(Vtm::Univ),

        TmData::BoolTy => Ok(Vtm::BoolTy),
        TmData::BoolLit { b } => Ok(Vtm::Bool { b: *b }),

        TmData::IntTy => Ok(Vtm::IntTy),
        TmData::IntLit { i } => Ok(Vtm::Int { i: *i }),

        TmData::StrTy => Ok(Vtm::StrTy),
        TmData::StrLit { s } => Ok(Vtm::Str { s: s.clone() }),

        TmData::FunTy { args, body: ty } => Ok(Vtm::FunTy {
            args: args
                .iter()
                .map(|arg| eval(env, arg))
                .collect::<Result<Vec<_>, _>>()?,
            body: Rc::new(eval(env, ty)?),
        }),
        TmData::FunLit { args, body } => Ok(Vtm::Fun {
            body: FunData {
                env: args.iter().try_fold(env.clone(), |env0, arg| {
                    let vtm = eval(env, arg)?;

                    Ok(env0.with(vtm))
                })?,
                body: body.as_ref().clone(),
            },
        }),
        TmData::FunApp { head, args } => app(
            &eval(env, head)?,
            args.iter()
                .map(|arg| eval(env, arg))
                .collect::<Result<Vec<_>, _>>()?,
        ),

        TmData::Var { index } => Ok(env.get(*index).clone()),
    }
}

fn app(head: &Vtm, args: Vec<Vtm>) -> Result<Vtm, EvalError> {
    match head {
        Vtm::Ntm { ntm } => Ok(Vtm::Ntm {
            ntm: Ntm::FunApp {
                head: Rc::new(ntm.clone()),
                args,
            },
        }),
        Vtm::Fun { body } => body.app(&args),
        _ => panic!("invalid application?!"),
    }
}

/// Neutral terms
#[derive(Clone)]
pub enum Ntm {
    Var { level: usize },

    FunApp { head: Rc<Ntm>, args: Vec<Vtm> },
}

impl Display for Vtm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Vtm::Ntm { ntm } => ntm.fmt(f),
            Vtm::Univ => "Type".fmt(f),
            Vtm::BoolTy => "Bool".fmt(f),
            Vtm::Bool { b } => b.fmt(f),
            Vtm::IntTy => "Int".fmt(f),
            Vtm::Int { i } => i.fmt(f),
            Vtm::StrTy => "Str".fmt(f),
            Vtm::Str { s } => s.fmt(f),
            Vtm::FunTy { args, body } => format!(
                "({}) -> {}",
                args.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                body
            )
            .fmt(f),
            Vtm::Fun { body } => "#func".fmt(f),
        }
    }
}

impl Display for Ntm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ntm::Var { level } => format!("[lvl {}]", level).fmt(f),
            Ntm::FunApp { head, args } => format!(
                "{}({})",
                head,
                args.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
        }
    }
}
