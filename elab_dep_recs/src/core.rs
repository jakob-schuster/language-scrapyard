use itertools::Itertools;

use crate::util::{self, Env, Located, RecField};

use std::{fmt::Display, rc::Rc};

pub mod matcher;

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
    /// Variables
    Var {
        index: usize,
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

    /// A record type; identified by the set of names and types of its fields
    RecTy {
        fields: Vec<RecField<Ty>>,
    },
    /// A record literal; has a set of named fields with term values
    RecLit {
        fields: Vec<RecField<Tm>>,
    },
    /// Record projection, also called field access
    RecProj {
        tm: Rc<Tm>,
        name: String,
    },

    Match {
        tm: Rc<Tm>,
        branches: Vec<Branch>,
    },
}

#[derive(Clone)]
pub struct Branch {
    /// A function from an environment to a list of new bindings, or a failure to match
    pub matcher: Rc<dyn matcher::Matcher>,

    /// A body to evaluate if the match function succeeds
    pub body: Tm,
}

impl Tm {
    fn is_bound(&self, index: Index) -> bool {
        match &self.data {
            TmData::Let { head, body } => head.is_bound(index) || body.is_bound(index),
            TmData::Var { index: index1 } => index1.eq(&index),

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

            TmData::RecTy { fields } => fields.iter().any(|field| field.data.is_bound(index)),
            TmData::RecLit { fields } => fields.iter().any(|field| field.data.is_bound(index)),
            TmData::RecProj { tm, name } => tm.is_bound(index),

            // todo: finish is_bound for match. haven't done this because it
            // looks like this whole function was only used for resugaring
            // in brendan's implementation
            TmData::Match { tm, branches } => todo!(),
        }
    }
}

/// Value terms
#[derive(Clone)]
pub enum Vtm {
    // Neutral terms
    Ntm { ntm: Ntm },

    Univ,

    // Equivalent to all other types
    AnyTy,

    BoolTy,
    Bool { b: bool },

    IntTy,
    Int { i: i32 },

    StrTy,
    Str { s: String },

    FunTy { args: Vec<Vtm>, body: Rc<Vtm> },
    Fun { body: FunData },

    RecTy { fields: Vec<RecField<Vtm>> },
    Rec { fields: Vec<RecField<Vtm>> },
}
pub type Vty = Vtm;

impl PartialEq for Vtm {
    fn eq(&self, other: &Self) -> bool {
        // takes a field name, and looks it up in the list of fields,
        // returning the type if one is found
        let get = |fields: &[RecField<Vtm>], name: &str| -> Option<Vtm> {
            let mut out = None;
            for field in fields {
                if field.name.eq(name) {
                    out = Some(field.data.clone())
                }
            }
            out
        };

        match (self, other) {
            (Vtm::AnyTy, _) | (_, Vtm::AnyTy) => true,

            // trivially equal
            (Vtm::Univ, Vtm::Univ)
            | (Vtm::BoolTy, Vtm::BoolTy)
            | (Vtm::IntTy, Vtm::IntTy)
            | (Vtm::StrTy, Vtm::StrTy) => true,
            (Vtm::Bool { b: b1 }, Vtm::Bool { b: b2 }) => b1.eq(b2),
            (Vtm::Int { i: i1 }, Vtm::Int { i: i2 }) => i1.eq(i2),
            (Vtm::Str { s: s1 }, Vtm::Str { s: s2 }) => s1.eq(s2),

            // record types and records are equivalent if all fields are the same
            (Vtm::RecTy { fields: fields1 }, Vtm::RecTy { fields: fields2 }) => {
                let mut names = fields1
                    .iter()
                    .chain(fields2)
                    .map(|a| a.name.clone())
                    .unique();

                fields1.len().eq(&fields2.len())
                    && names.all(|name| match (get(&fields1, &name), get(&fields2, &name)) {
                        // both recs must contain all fields, and the types must be equivalent
                        (Some(vty1), Some(vty2)) => vty1.equiv(&vty2),
                        // any fields missing is a type error
                        _ => false,
                    })
            }
            (Vtm::Rec { fields: fields1 }, Vtm::Rec { fields: fields2 }) => {
                let mut names = fields1
                    .iter()
                    .chain(fields2)
                    .map(|a| a.name.clone())
                    .unique();

                fields1.len().eq(&fields2.len())
                    && names.all(|name| match (get(&fields1, &name), get(&fields2, &name)) {
                        // both recs must contain all fields, and the types must be equivalent
                        (Some(vty1), Some(vty2)) => vty1.equiv(&vty2),
                        // any fields missing is a type error
                        _ => false,
                    })
            }

            _ => false,
        }
    }
}

impl Vtm {
    pub fn equiv(&self, other: &Vtm) -> bool {
        // takes a field name, and looks it up in the list of fields,
        // returning the type if one is found
        let get = |fields: &[RecField<Vtm>], name: &str| -> Option<Vtm> {
            let mut out = None;
            for field in fields {
                if field.name.eq(name) {
                    out = Some(field.data.clone())
                }
            }
            out
        };

        match (self, other) {
            // Any is equivalent to everything
            (Vtm::AnyTy, _) | (_, Vtm::AnyTy) => true,

            // trivially equivalent
            (Vtm::Univ, Vtm::Univ)
            | (Vtm::BoolTy, Vtm::BoolTy)
            | (Vtm::IntTy, Vtm::IntTy)
            | (Vtm::StrTy, Vtm::StrTy) => true,

            // equivalent if argument types are the same
            // and return types are the same
            (
                Vtm::FunTy {
                    args: args1,
                    body: body1,
                },
                Vtm::FunTy {
                    args: args2,
                    body: body2,
                },
            ) => {
                args1.len().eq(&args2.len())
                    && args1.iter().zip(args2).all(|(arg1, arg2)| arg1.equiv(arg2))
                    && body1.equiv(body2)
            }

            // equivalent if all fields are the same
            (Vtm::RecTy { fields: fields1 }, Vtm::RecTy { fields: fields2 }) => {
                let mut names = fields1
                    .iter()
                    .chain(fields2)
                    .map(|a| a.name.clone())
                    .unique();

                fields1.len().eq(&fields2.len())
                    && names.all(|name| match (get(&fields1, &name), get(&fields2, &name)) {
                        // both recs must contain all fields, and the types must be equivalent
                        (Some(vty1), Some(vty2)) => vty1.equiv(&vty2),
                        // any fields missing is a type error
                        _ => false,
                    })
            }

            _ => false,
        }
    }

    pub fn most_precise(&self, other: &Vtm) -> Vtm {
        // takes a field name, and looks it up in the list of fields,
        // returning the type if one is found
        let get = |fields: &[RecField<Vtm>], name: &str| -> Option<Vtm> {
            let mut out = None;
            for field in fields {
                if field.name.eq(name) {
                    out = Some(field.data.clone())
                }
            }
            out
        };

        match (self, other) {
            // if either type is Any, the other type is assumed more precise
            (Vtm::AnyTy, ty) | (ty, Vtm::AnyTy) => ty.clone(),

            // if both are Rec, take the most precise version of each field
            (Vtm::RecTy { fields: fields1 }, Vtm::RecTy { fields: fields2 }) => {
                Vtm::RecTy {
                    fields: fields1
                        .iter()
                        .chain(fields2)
                        .map(|RecField { name, .. }| name)
                        .unique()
                        .map(|name| match (get(fields1, name), get(fields2, name)) {
                            // when both Recs contain the field name, take the most precise definition
                            (Some(ty1), Some(ty2)) => {
                                RecField::new(name.clone(), ty1.most_precise(&ty2))
                            }
                            // this should never happen
                            _ => panic!(
                                "a record was missing a field?! both should have all fields?!"
                            ),
                        })
                        .collect::<Vec<_>>(),
                }
            }

            // all atomic combinations are equally precise (assuming they are equiv!)
            _ => self.clone(),
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
                env: env.clone(),
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

        TmData::RecTy { fields } => Ok(Vtm::RecTy {
            fields: fields
                .iter()
                .map(|field| Ok(RecField::new(field.name.clone(), eval(env, &field.data)?)))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        TmData::RecLit { fields } => Ok(Vtm::Rec {
            fields: fields
                .iter()
                .map(|field| Ok(RecField::new(field.name.clone(), eval(env, &field.data)?)))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        TmData::RecProj { tm, name } => {
            let vtm = eval(env, tm)?;

            match vtm {
                Vtm::Ntm { ntm } => Ok(Vtm::Ntm {
                    ntm: Ntm::RecProj {
                        tm: Rc::new(ntm),
                        name: name.clone(),
                    },
                }),
                Vtm::Rec { fields } => {
                    for field in fields {
                        if field.name.eq(name) {
                            return Ok(field.data);
                        }
                    }

                    panic!("trying to access a non-existent field?!")
                }

                _ => panic!("trying to project on non-record type?!"),
            }
        }
        TmData::Match { tm, branches } => {
            let vtm = eval(env, tm)?;

            // go through each branch
            for branch in branches {
                if let Some(vtms) = branch.matcher.evaluate(env, &vtm) {
                    // bind all the values in the env
                    let new_env = vtms
                        .iter()
                        .fold(env.clone(), |env0, vtm| env0.with(vtm.clone()));

                    // evaluate the arm, just for the first one that matches
                    return eval(&new_env, &branch.body);
                }
            }

            panic!("incomplete match?!")
        }
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

    RecProj { tm: Rc<Ntm>, name: String },
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

            Vtm::RecTy { fields } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
            Vtm::Rec { fields } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),

            Vtm::AnyTy => "Any".fmt(f),
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
            Ntm::RecProj { tm, name } => format!("{}.{}", tm, name).fmt(f),
        }
    }
}
