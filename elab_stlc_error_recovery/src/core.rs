use std::{fmt::Display, rc::Rc};

use crate::{prim::Prim, util::Env};

// names are used as hints for pretty printing binders and variables,
// but don't impact the equality of terms
type Name = String;

// binding structure of terms is represented in the core language by
// using numbers that represent the distance to a binder, instead of by the
// names attached to those binders

// De Bruijn index that represents a variable occurrence by the number of
// binders between the occurrence and the binder it refers to.
pub type Index = usize;

// De Bruijn level that represents a variable occurrence by the number of
// binders from the top of the environment to the binder that the occurrence
// refers to. These do not change their meaning as new bindings are added to
// the environment.
pub type Level = usize;

fn level_to_index(size: &usize, level: &Level) -> Index {
    size - level - 1
}

#[derive(Clone)]
pub enum Ty {
    FunTy { head: Rc<Ty>, body: Rc<Ty> },
    IntTy,
    BoolTy,

    // a type hole, used when recovering from errors
    UnknownTy,
}

#[derive(Clone)]
pub enum Tm {
    Var {
        index: Index,
    },
    Let {
        def_name: Name,
        def_ty: Ty,
        def: Rc<Tm>,
        body: Rc<Tm>,
    },
    FunLit {
        name: Name,
        param_ty: Ty,
        body: Rc<Tm>,
    },
    FunApp {
        head: Rc<Tm>,
        arg: Rc<Tm>,
    },
    IntLit {
        i: i32,
    },
    BoolLit {
        b: bool,
    },
    BoolElim {
        head: Rc<Tm>,
        tm1: Rc<Tm>,
        tm2: Rc<Tm>,
    },
    PrimApp {
        prim: Prim,
        args: Vec<Tm>,
    },
    ReportedError,
}

#[derive(Clone)]
enum Val {
    Neutral {
        neutral: Neutral,
    },
    FunLit {
        name: Name,
        param_ty: Ty,
        body: FunData,
    },
    IntLit {
        i: i32,
    },
    BoolLit {
        b: bool,
    },
}

#[derive(Clone)]
struct FunData {
    env: Env<Val>,
    body: Tm,
}

impl FunData {
    fn apply(&self, arg: &Val) -> Val {
        let env1 = self.env.with(arg.clone());
        eval(&env1, &self.body)
    }
}

#[derive(Clone)]
enum Neutral {
    Var {
        level: Level,
    },
    FunApp {
        head: Rc<Neutral>,
        arg: Rc<Val>,
    },
    BoolElim {
        head: Rc<Neutral>,
        tm1: Rc<Val>,
        tm2: Rc<Val>,
    },
    PrimApp {
        prim: Prim,
        args: Vec<Val>,
    },
    ReportedError,
}

// eliminators

fn fun_app(head: &Val, arg: &Val) -> Val {
    match head {
        Val::Neutral { neutral } => Val::Neutral {
            neutral: Neutral::FunApp {
                head: Rc::new(neutral.clone()),
                arg: Rc::new(arg.clone()),
            },
        },
        Val::FunLit {
            name,
            param_ty,
            body,
        } => body.apply(arg),
        _ => panic!("expected function!"),
    }
}

fn bool_elim(head: &Val, val1: &Val, val2: &Val) -> Val {
    match head {
        Val::Neutral { neutral } => Val::Neutral {
            neutral: Neutral::BoolElim {
                head: Rc::new(neutral.clone()),
                tm1: Rc::new(val1.clone()),
                tm2: Rc::new(val2.clone()),
            },
        },
        Val::BoolLit { b } => match b {
            true => val1.clone(),
            false => val2.clone(),
        },
        _ => panic!("expected boolean"),
    }
}

fn prim_app(prim: &Prim, vals: &[Val]) -> Val {
    todo!()
}

fn eval(env: &Env<Val>, tm: &Tm) -> Val {
    match tm {
        Tm::Var { index } => env.lookup(index).clone(),
        Tm::Let {
            def_name,
            def_ty,
            def,
            body,
        } => {
            let val = eval(env, def);
            eval(&env.with(val), body)
        }
        Tm::FunLit {
            name,
            param_ty,
            body,
        } => Val::FunLit {
            name: name.clone(),
            param_ty: param_ty.clone(),
            body: FunData {
                env: env.clone(),
                body: body.as_ref().clone(),
            },
        },
        Tm::FunApp { head, arg } => {
            let head_val = eval(env, head);
            let arg_val = eval(env, arg);

            fun_app(&head_val, &arg_val)
        }
        Tm::IntLit { i } => Val::IntLit { i: *i },
        Tm::BoolLit { b } => Val::BoolLit { b: *b },
        Tm::BoolElim { head, tm1, tm2 } => {
            let head_val = eval(env, head);
            let val1 = eval(env, tm1);
            let val2 = eval(env, tm2);

            bool_elim(&head_val, &val1, &val2)
        }
        Tm::PrimApp { prim, args } => prim_app(
            prim,
            &args.iter().map(|arg| eval(env, arg)).collect::<Vec<_>>(),
        ),
        Tm::ReportedError => Val::Neutral {
            neutral: Neutral::ReportedError,
        },
    }
}

// Check if two types are compatible with each other
pub fn equate_tys(ty1: &Ty, ty2: &Ty) -> bool {
    match (ty1, ty2) {
        (Ty::UnknownTy, _) | (_, Ty::UnknownTy) => true,
        (
            Ty::FunTy {
                head: param_ty1,
                body: body_ty1,
            },
            Ty::FunTy {
                head: param_ty2,
                body: body_ty2,
            },
        ) => equate_tys(param_ty1, param_ty2) && equate_tys(body_ty1, body_ty2),
        (Ty::IntTy, Ty::IntTy) => true,
        (Ty::BoolTy, Ty::BoolTy) => true,
        _ => false,
    }
}

// The "meet" of two types picks the most specific of two compatible types,
// which is important for propagating as much type information as we can when
// inferring the type of conditionals. This was inspired by section 2.1.6 of
// "Total Type Error Localization and Recovery with Holes" by Zhao et. al.
pub fn meet_tys(ty1: &Ty, ty2: &Ty) -> Option<Ty> {
    match (ty1, ty2) {
        (Ty::UnknownTy, ty) | (ty, Ty::UnknownTy) => Some(ty.clone()),
        (
            Ty::FunTy {
                head: param_ty1,
                body: body_ty1,
            },
            Ty::FunTy {
                head: param_ty2,
                body: body_ty2,
            },
        ) => {
            let param_ty = meet_tys(param_ty1, param_ty2)?;
            let body_ty = meet_tys(body_ty1, body_ty2)?;

            Some(Ty::FunTy {
                head: Rc::new(param_ty.clone()),
                body: Rc::new(body_ty.clone()),
            })
        }
        (Ty::IntTy, Ty::IntTy) => Some(Ty::IntTy),
        (Ty::BoolTy, Ty::BoolTy) => Some(Ty::BoolTy),
        _ => None,
    }
}

impl Display for Tm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tm::Var { index } => format!("#{}", index).fmt(f),
            Tm::Let {
                def_name,
                def_ty,
                def,
                body,
            } => format!("let {}: {} = {} in {}", def_name, def_ty, def, body).fmt(f),
            Tm::FunLit {
                name,
                param_ty,
                body,
            } => format!("|{}: {}| {}", name, param_ty, body).fmt(f),
            Tm::FunApp { head, arg } => format!("({})({})", head, arg).fmt(f),
            Tm::IntLit { i } => i.fmt(f),
            Tm::BoolLit { b } => b.fmt(f),
            Tm::BoolElim { head, tm1, tm2 } => {
                format!("if {} then {} else {}", head, tm1, tm2).fmt(f)
            }
            Tm::PrimApp { prim, args } => format!(
                "{}[{}]",
                prim,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )
            .fmt(f),
            Tm::ReportedError => "Error".fmt(f),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::FunTy { head, body } => format!("{} -> {}", head, body).fmt(f),
            Ty::IntTy => "Int".fmt(f),
            Ty::BoolTy => "Bool".fmt(f),
            Ty::UnknownTy => "Unknown".fmt(f),
        }
    }
}
