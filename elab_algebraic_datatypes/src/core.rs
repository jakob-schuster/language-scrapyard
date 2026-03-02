use std::{collections::HashMap, rc::Rc};

use crate::prim::Prim;

// De Bruijn index that represents a variable occurrence by the number of
// binders between the occurrence and the binder it refers to.
pub type Index = usize;

#[derive(Clone)]
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

#[derive(Clone)]
enum Val {
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
    StructProj {
        head: Rc<Tm>,
        field: String,
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

// an environment of values
#[derive(Clone)]
struct Env {
    vals: Vec<Val>,
}

impl Env {
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
    fn eval(&self, env: &Env) -> Val {
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
            Tm::PrimApp { prim, args } => match prim {
                Prim::BoolEq => todo!(),

                Prim::IntEq => todo!(),
                Prim::IntAdd => todo!(),
                Prim::IntSub => todo!(),
                Prim::IntMul => todo!(),
                Prim::IntNeg => todo!(),
            },
        }
    }
}
