use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{prim::Prim, surface};

// De Bruijn index that represents a variable occurrence by the number of
// binders between the occurrence and the binder it refers to.
pub type Index = usize;

#[derive(Clone, PartialEq)]
pub enum Tm {
    Var {
        i: usize,
    },

    UnivTy,

    BoolTy,
    Bool {
        b: bool,
    },

    NumTy,
    Num {
        n: i32,
    },

    EnumTy {
        // each variant contains a number of fields
        // each field is named, and has a type
        variants: HashMap<String, HashMap<String, Tm>>,
    },
    Enum {
        head: Rc<Tm>,
        variant: String,
        fields: HashMap<String, Tm>,
    },

    StructTy {
        // a struct contains a number of fields
        // each field is named, and has a type
        fields: HashMap<String, Tm>,
    },
    Struct {
        fields: HashMap<String, Tm>,
    },
    StructProj {
        head: Rc<Tm>,
        field: String,
    },

    Let {
        tm: Rc<Tm>,
        rest: Rc<Tm>,
    },

    FunTy {
        args: Vec<Tm>,
        body: Rc<Tm>,
    },
    Fun {
        body: Rc<Tm>,
    },
    FunApp {
        head: Rc<Tm>,
        args: Vec<Tm>,
    },

    ErrorTy,
    Error,

    IfThenElse {
        guard: Rc<Tm>,
        tm1: Rc<Tm>,
        tm2: Rc<Tm>,
    },

    PrimApp {
        prim: Prim,
        args: Vec<Tm>,
    },
}

#[derive(Clone, PartialEq)]
pub enum Val {
    Neutral {
        neutral: Neutral,
    },

    UnivTy,

    BoolTy,
    Bool {
        b: bool,
    },

    NumTy,
    Num {
        n: i32,
    },

    EnumTy {
        // each variant contains a number of fields
        variants: HashMap<String, HashMap<String, Val>>,
    },
    Enum {
        // enum vals have forgotten where they come from -
        // they don't need to know anymore
        variant: String,
        fields: HashMap<String, Val>,
    },

    StructTy {
        fields: HashMap<String, Val>,
    },
    Struct {
        fields: HashMap<String, Val>,
    },

    FunTy {
        args: Vec<Val>,
        body: Rc<Val>,
    },
    Fun {
        body: Rc<Tm>,
    },

    ErrorTy,
    Error,
}

impl Val {
    pub fn equiv_ty(&self, other: &Val) -> bool {
        match (self, other) {
            (Val::UnivTy, Val::UnivTy) | (Val::BoolTy, Val::BoolTy) | (Val::NumTy, Val::NumTy) => {
                true
            }

            (
                Val::EnumTy {
                    variants: self_variants,
                },
                Val::EnumTy {
                    variants: other_variants,
                },
            ) => {
                if self_variants.len() != other_variants.len() {
                    false
                } else {
                    for (variant, fields) in self_variants {
                        match other_variants.get(variant) {
                            Some(other_fields) => {
                                if fields.len() != other_fields.len() {
                                    return false;
                                }

                                for (name, field) in fields {
                                    match other_fields.get(name) {
                                        Some(other_field) => {
                                            if !field.equiv_ty(other_field) {
                                                return false;
                                            }
                                        }
                                        None => return false,
                                    }
                                }
                            }
                            None => return false,
                        }
                    }

                    true
                }
            }

            (
                Val::StructTy {
                    fields: self_fields,
                },
                Val::StructTy {
                    fields: other_fields,
                },
            ) => {
                if self_fields.len() != other_fields.len() {
                    false
                } else {
                    for (name, field) in self_fields {
                        match other_fields.get(name) {
                            Some(other_field) => {
                                if !field.equiv_ty(other_field) {
                                    return false;
                                }
                            }
                            None => return false,
                        }
                    }

                    true
                }
            }

            _ => false,
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Neutral { neutral } => todo!(),
            Val::UnivTy => "Type".fmt(f),
            Val::BoolTy => "Bool".fmt(f),
            Val::Bool { b } => b.fmt(f),
            Val::NumTy => "Num".fmt(f),
            Val::Num { n } => n.fmt(f),
            Val::EnumTy { variants } => format!(
                "Enum {{{}}}",
                variants
                    .iter()
                    .map(|(name, fields)| format!(
                        "{} {{{}}}",
                        name,
                        fields
                            .iter()
                            .map(|(name, ty)| format!("{}: {}", name, ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
                    .collect::<Vec<_>>()
                    .join(" | ")
            )
            .fmt(f),
            Val::Enum { variant, fields } => format!(
                "enum {}.{{{}}}",
                variant,
                fields
                    .iter()
                    .map(|(name, field)| format!("{}: {}", name, field))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
            Val::StructTy { fields } => format!(
                "Struct {{{}}}",
                fields
                    .iter()
                    .map(|(name, field)| format!("{}: {}", name, field))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
            Val::Struct { fields } => format!(
                "struct {{{}}}",
                fields
                    .iter()
                    .map(|(name, field)| format!("{}: {}", name, field))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
            Val::FunTy { args, body } => format!(
                "Fn({}): {}",
                args.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                body
            )
            .fmt(f),
            Val::Fun { body } => format!("fn(?): ?").fmt(f),
            Val::ErrorTy => "Error".fmt(f),
            Val::Error => "error".fmt(f),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Neutral {
    Var { i: usize },
}

// an environment of values
#[derive(Clone)]
pub struct Env {
    vals: Vec<Val>,
}

impl Env {
    pub fn from_ctx(ctx: &surface::Context) -> Env {
        let mut tms = ctx.tys.values().collect::<Vec<_>>();
        tms.sort_by_key(|(index, _, _)| index);
        let sorted_tms = tms
            .into_iter()
            .map(|(_, _, tm)| tm.clone())
            .collect::<Vec<_>>();

        Env { vals: sorted_tms }
    }

    // the empty environment
    fn empty() -> Env {
        Env { vals: Vec::new() }
    }

    fn get(&self, i: &usize) -> Val {
        self.vals.get(*i).unwrap().clone()
    }

    // creates a new environment with the value
    fn with(&self, val: &Val) -> Env {
        let mut vals = self.vals.clone();
        vals.push(val.clone());

        Env { vals }
    }
}

impl Tm {
    pub fn eval(&self, env: &Env) -> Val {
        match self {
            Tm::Var { i } => env.get(i),

            Tm::UnivTy => Val::UnivTy,

            Tm::BoolTy => Val::BoolTy,
            Tm::Bool { b } => Val::Bool { b: *b },

            Tm::NumTy => Val::NumTy,
            Tm::Num { n } => Val::Num { n: *n },

            Tm::EnumTy { variants } => Val::EnumTy {
                variants: variants
                    .iter()
                    .map(|(name, fields)| {
                        (
                            name.clone(),
                            fields
                                .iter()
                                .map(|(name, tm)| (name.clone(), tm.eval(env)))
                                .collect(),
                        )
                    })
                    .collect(),
            },
            Tm::Enum {
                head: _,
                variant,
                fields,
            } => Val::Enum {
                variant: variant.clone(),
                fields: fields
                    .iter()
                    .map(|(name, tm)| (name.clone(), tm.eval(env)))
                    .collect(),
            },
            Tm::StructTy { fields } => Val::StructTy {
                fields: fields
                    .iter()
                    .map(|(name, tm)| (name.clone(), tm.eval(env)))
                    .collect(),
            },
            Tm::Struct { fields } => Val::Struct {
                fields: fields
                    .iter()
                    .map(|(name, tm)| (name.clone(), tm.eval(env)))
                    .collect(),
            },
            Tm::StructProj { head, field } => match head.eval(env) {
                Val::Struct { fields } => match fields.get(field) {
                    Some(field) => field.clone(),
                    None => panic!("expected field '{}'?", field),
                },
                _ => panic!("expected struct?!"),
            },

            Tm::Let { tm, rest } => rest.eval(&env.with(&tm.eval(env))),
            Tm::FunTy { args, body } => Val::FunTy {
                args: args.iter().map(|tm| tm.eval(env)).collect(),
                body: Rc::new(body.eval(env)),
            },
            Tm::Fun { body } => Val::Fun { body: body.clone() },
            Tm::FunApp { head, args } => match head.eval(env) {
                Val::Fun { body } => body.eval(
                    &args
                        .iter()
                        .fold(env.clone(), |env0, arg| env0.with(&arg.eval(env))),
                ),
                _ => panic!("expected fun?!"),
            },
            Tm::ErrorTy => Val::ErrorTy,
            Tm::Error => Val::Error,
            Tm::IfThenElse { guard, tm1, tm2 } => match guard.eval(env) {
                Val::Bool { b } => {
                    if b {
                        tm1.eval(env)
                    } else {
                        tm2.eval(env)
                    }
                }
                _ => panic!("expected bool?!"),
            },
            Tm::PrimApp { prim, args } => {
                prim.apply(&args.iter().map(|tm| tm.eval(env)).collect::<Vec<_>>())
            }
        }
    }
}
