// Bidirectional elaboration and evaluation for System F, based on brendan's language garden implementation

use core::semantics::{eval_ty, quote_vty};
use std::{collections::HashMap, rc::Rc};

// Types in the surface language
enum Ty {
    Name(String),
    TyFunType { params: Vec<String>, t: Rc<Ty> },
    FunType { t0: Rc<Ty>, t1: Rc<Ty> },
}

// Terms in the surface language
enum Tm {
    Name(String),
    Let {
        name: String,
        params: Vec<Param>,
        def_body_ty: Option<Ty>,
        def_body: Rc<Tm>,
        body: Rc<Tm>,
    },
    Ann {
        tm: Rc<Tm>,
        ty: Ty,
    },
    FunLit {
        params: Vec<Param>,
        tm: Rc<Tm>,
    },
    BoolLit(bool),
    IntLit(i32),
    App {
        tm: Rc<Tm>,
        arg: Rc<Arg>,
    },
    IfThenElse {
        guard: Rc<Tm>,
        tm0: Rc<Tm>,
        tm1: Rc<Tm>,
    },
    Op2 {
        op: Op2,
        tm0: Rc<Tm>,
        tm1: Rc<Tm>,
    },
    Op1 {
        op: Op1,
        tm: Rc<Tm>,
    },
}

// Parameters, with optional type annotations
enum Param {
    TyParam(String),
    Param { binder: String, ty: Option<Ty> },
}

enum Arg {
    TyArg(Ty),
    Arg(Tm),
}

enum Op2 {
    Eq,
    Add,
    Sub,
    Mul,
}

enum Op1 {
    Neg,
}

// elaboration
#[derive(Clone)]
struct Env<A> {
    vec: Vec<A>,
}

impl<A> Env<A> {
    fn iter(&self) -> std::slice::Iter<'_, A> {
        self.vec.iter()
    }

    fn get(&self, i: usize) -> &A {
        match self.vec.get(i) {
            Some(a) => a,
            None => panic!("Bad index in env!"),
        }
    }
}

impl<A: Clone> Env<A> {
    fn with(&self, a: A) -> Env<A> {
        let mut vec = self.vec.clone();
        vec.push(a);

        Env { vec }
    }
}

impl<A> Default for Env<A> {
    fn default() -> Self {
        Env { vec: vec![] }
    }
}

#[derive(Default)]
struct Context {
    ty_size: usize,
    ty_names: Env<String>,
    ty_env: Env<core::semantics::Vty>,
    tm_tys: Env<(String, core::semantics::Vty)>,
}

impl Context {
    // The type variable that will be bound after calling extend_ty
    fn next_ty_var(&self) -> core::semantics::Vty {
        core::semantics::Vty::Neu(core::semantics::Nty::Var(self.ty_size))
    }

    // Extend the context with a type binding
    fn extend_ty(&self, name: String) -> Context {
        Context {
            ty_size: self.ty_size + 1,
            ty_names: self.ty_names.with(name),
            ty_env: self
                .ty_env
                .with(core::semantics::Vty::Neu(core::semantics::Nty::Var(
                    self.ty_size,
                ))),
            tm_tys: self.tm_tys.clone(),
        }
    }

    // Extend the context with a term binding
    fn extend_tm(&self, name: &String, vty: &core::semantics::Vty) -> Context {
        Context {
            ty_size: self.ty_size,
            ty_names: self.ty_names.clone(),
            ty_env: self.ty_env.clone(),
            tm_tys: self.tm_tys.with((name.clone(), vty.clone())),
        }
    }

    // Lookup a type name in the context
    fn lookup_ty(&self, name: &str) -> Option<core::Index> {
        self.ty_names
            .iter()
            .enumerate()
            .flat_map(|(i, a)| match name.eq(a) {
                true => Some(i),
                false => None,
            })
            .next()
    }

    // Lookup a term name in the context
    fn lookup_tm(&self, name: &String) -> Option<(core::Index, core::semantics::Vty)> {
        self.tm_tys
            .iter()
            .enumerate()
            .flat_map(|(i, (name1, vty))| match name.eq(name1) {
                true => Some((i, vty.clone())),
                false => None,
            })
            .next()
    }

    fn eval_ty(&self, ty: &core::Ty) -> core::semantics::Vty {
        core::semantics::eval_ty(self.ty_env.clone(), ty.clone())
    }

    fn quote_vty(&self, vty: &core::semantics::Vty) -> core::Ty {
        core::semantics::quote_vty(self.ty_size, vty)
    }

    fn pp_ty(&self, f: std::fmt::Formatter, ty: core::Ty) {
        core::pp_ty(&self.ty_names, f, ty)
    }
}

// Elaboration errors

// An error that will be raised if there was a problem in the surface syntax,
// usually as a result of type errors. This is normal, and should be rendered
// nicely to the programmer.
type ElabError = String;

fn equate_vtys(ctx: &Context, vty1: &core::semantics::Vty, vty2: &core::semantics::Vty) {
    todo!()
}

// Bidirectional type checking

// The algorithm is structured bidirectionally, divided into mutually
// recursive checking and inference modes. By supplying type
// annotations as early as possible using the checking mode, we can improve
// the locality of type errors. We can also extend the type system with
// advanced features like dependent types, higher rank types, and subtyping
// while maintaining decidability by allowing the programmer to supply
// annotations where necessary.

fn elab_ty(ctx: &Context, ty: &Ty) -> core::Ty {
    match ty {
        Ty::Name(name) => match ctx.lookup_ty(name) {
            Some(index) => core::Ty::Var(index),
            None => match &name[..] {
                "Bool" => core::Ty::BoolType,
                "Int" => core::Ty::IntType,
                _ => panic!("unbound type {name}!"),
            },
        },
        Ty::TyFunType { params, t } => {
            fn go(ctx: &Context, names: &[String], body_ty: &Ty) -> core::Ty {
                match names {
                    [] => elab_ty(ctx, body_ty),
                    [name, rest @ ..] => core::Ty::TyFunType {
                        name: name.clone(),
                        ty: Rc::new(go(ctx, rest, body_ty)),
                    },
                }
            }

            go(ctx, params, t)
        }
        Ty::FunType { t0, t1 } => core::Ty::FunType {
            ty0: Rc::new(elab_ty(ctx, t0)),
            ty1: Rc::new(elab_ty(ctx, t1)),
        },
    }
}

fn elab_check(ctx: &Context, tm: &Tm, vty: &core::semantics::Vty) -> core::Tm {
    match tm {
        Tm::Let {
            name,
            params,
            def_body_ty,
            def_body,
            body,
        } => {
            let (def, def_vty) = elab_infer_fun_lit(ctx, params, def_body_ty, def_body);
            let (body, body_ty) = elab_infer(ctx, tm);

            core::Tm::Let {
                name: name.clone(),
                ty: quote_vty(ctx.ty_size, &vty),
                tm0: Rc::new(def),
                tm1: Rc::new(body),
            }
        }
        Tm::FunLit { params, tm } => elab_check_fun_lit(ctx, params, tm, vty),
        Tm::IfThenElse { guard, tm0, tm1 } => {
            let head = elab_check(ctx, guard, vty);
            let tm0_c = elab_check(ctx, tm0, vty);
            let tm1_c = elab_check(ctx, tm1, vty);

            core::Tm::BoolElim {
                guard: Rc::new(head),
                tm0: Rc::new(tm0_c),
                tm1: Rc::new(tm1_c),
            }
        }
        _ => {
            let (tm_c, vty1) = elab_infer(ctx, tm);
            equate_vtys(ctx, vty, &vty1);

            tm_c
        }
    }
}

// Elaborate a surface term into a core term, inferring its type.
fn elab_infer(ctx: &Context, tm: &Tm) -> (core::Tm, core::semantics::Vty) {
    match tm {
        Tm::Name(name) => match ctx.lookup_tm(name) {
            Some((index, vty)) => (core::Tm::Var(index), vty),
            None => panic!("unbound name {name}"),
        },
        Tm::Let {
            name,
            params,
            def_body_ty,
            def_body,
            body,
        } => {
            let (def, def_vty) = elab_infer_fun_lit(ctx, params, def_body_ty, def_body);
            let (body, body_ty) = elab_infer(&ctx.extend_tm(name, &def_vty), body);

            (
                core::Tm::Let {
                    name: name.clone(),
                    ty: quote_vty(ctx.ty_size, &def_vty),
                    tm0: Rc::new(def),
                    tm1: Rc::new(body),
                },
                body_ty,
            )
        }
        Tm::Ann { tm, ty } => {
            let ty0 = elab_ty(ctx, ty);
            let vty = ctx.eval_ty(&ty0);

            (elab_check(ctx, tm, &vty), vty)
        }
        Tm::FunLit { params, tm } => todo!(),
        Tm::BoolLit(b) => (core::Tm::BoolLit(*b), core::semantics::Vty::BoolType),
        Tm::IntLit(i) => (core::Tm::IntLit(*i), core::semantics::Vty::IntType),
        Tm::App { tm, arg } => match (*arg).as_ref() {
            Arg::TyArg(ty) => {
                let (head, head_ty) = elab_infer(ctx, tm);
                let body_ty = match head_ty {
                    core::semantics::Vty::TyFunType { name, f: body_ty } => body_ty,
                    head_vty => panic!("mismatched types"),
                };

                let arg = elab_ty(ctx, &ty);

                (
                    core::Tm::TyFunApp {
                        tm: Rc::new(head),
                        ty: arg.clone(),
                    },
                    body_ty(&ctx.eval_ty(&arg)),
                )
            }
            Arg::Arg(arg_tm) => {
                let (head, head_ty) = elab_infer(ctx, tm);
                let (param_ty, body_ty) = match head_ty {
                    core::semantics::Vty::FunType { ty0, ty1 } => (ty0, ty1.as_ref().clone()),
                    head_vty => panic!("mismatched types. expected: function. found: "),
                };

                let arg = elab_check(ctx, tm, param_ty.as_ref());

                (
                    core::Tm::FunApp {
                        tm0: Rc::new(head),
                        tm1: Rc::new(arg),
                    },
                    body_ty,
                )
            }
        },
        Tm::IfThenElse { guard, tm0, tm1 } => panic!("ambiguous if expression"),
        Tm::Op2 { op, tm0, tm1 } => {
            let (ctm0, vty0) = elab_infer(ctx, tm0);
            let (ctm1, vty1) = elab_infer(ctx, tm1);
            equate_vtys(ctx, &vty0, &vty1);

            let (prim, vty) = match op {
                Op2::Eq => match vty0 {
                    core::semantics::Vty::BoolType => (
                        core::semantics::prim::Prim::BoolEq,
                        core::semantics::Vty::BoolType,
                    ),
                    core::semantics::Vty::IntType => (
                        core::semantics::prim::Prim::IntEq,
                        core::semantics::Vty::IntType,
                    ),
                    _ => panic!("unsupported type"),
                },
                Op2::Add => (
                    core::semantics::prim::Prim::IntAdd,
                    core::semantics::Vty::BoolType,
                ),
                Op2::Sub => (
                    core::semantics::prim::Prim::IntSub,
                    core::semantics::Vty::IntType,
                ),
                Op2::Mul => (
                    core::semantics::prim::Prim::IntMul,
                    core::semantics::Vty::IntType,
                ),
            };

            (
                core::Tm::PrimApp {
                    prim,
                    tms: vec![ctm0, ctm1],
                },
                vty,
            )
        }
        Tm::Op1 { op, tm } => {
            let (ctm0, vty0) = elab_infer(ctx, tm);

            let (prim, vty) = match op {
                Op1::Neg => (
                    core::semantics::prim::Prim::IntNeg,
                    core::semantics::Vty::IntType,
                ),
            };

            (
                core::Tm::PrimApp {
                    prim: core::semantics::prim::Prim::IntNeg,
                    tms: vec![ctm0],
                },
                vty,
            )
        }
    }
}

// Elaborate a function literal into a core term, given an expected type.
fn elab_check_fun_lit(
    ctx: &Context,
    params: &[Param],
    body: &Tm,
    vty: &core::semantics::Vty,
) -> core::Tm {
    match (params, vty) {
        ([], vty) => elab_check(ctx, body, vty),

        ([Param::TyParam(name), rest @ ..], core::semantics::Vty::TyFunType { f, .. }) => {
            let ty_var = ctx.next_ty_var();
            let body_tm = elab_check_fun_lit(&ctx.extend_ty(name.clone()), rest, body, &f(&ty_var));

            core::Tm::TyFunLit {
                name: name.clone(),
                tm: Rc::new(body_tm),
            }
        }

        (
            [Param::Param { binder: name, ty }, rest @ ..],
            core::semantics::Vty::FunType { ty0, ty1 },
        ) => match ty {
            Some(param_ty) => {
                let param_cty = elab_ty(ctx, param_ty);
                let param_vty = eval_ty(ctx.ty_env.clone(), param_cty.clone());
                equate_vtys(ctx, &param_vty, ty0);

                core::Tm::FunLit {
                    name: name.clone(),
                    ty: param_cty,
                    tm: Rc::new(elab_check_fun_lit(
                        &ctx.extend_tm(name, vty),
                        params,
                        body,
                        vty,
                    )),
                }
            }
            None => {
                let body_tm = elab_check_fun_lit(&ctx.extend_tm(name, ty0), rest, body, ty1);

                core::Tm::FunLit {
                    name: name.clone(),
                    ty: ctx.quote_vty(ty0),
                    tm: Rc::new(body_tm),
                }
            }
        },

        ([Param::TyParam(name), ..], _) => panic!("unexpected type parameter"),
        ([Param::Param { binder: name, .. }, ..], _) => panic!("unexpected parameter"),
    }
}

// Elaborate a function literal into a core term, inferring its type.
fn elab_infer_fun_lit(
    ctx: &Context,
    params: &[Param],
    body_ty: &Option<Ty>,
    body: &Tm,
) -> (core::Tm, core::semantics::Vty) {
    fn go(
        ctx: &Context,
        params: &[Param],
        body_ty: &Option<Ty>,
        body: &Tm,
    ) -> (core::Tm, core::Ty) {
        match (params, body_ty) {
            ([], Some(body_ty)) => {
                let body_cty = elab_ty(ctx, &body_ty);
                let body_vty = eval_ty(ctx.ty_env.clone(), body_cty.clone());
                (elab_check(ctx, body, &body_vty), body_cty)
            }
            ([], None) => {
                let (body_ctm, body_vty) = elab_infer(ctx, body);
                (body_ctm, ctx.quote_vty(&body_vty))
            }
            ([Param::TyParam(name), rest @ ..], body_ty) => {
                let (body, body_ty) = go(&ctx.extend_ty(name.clone()), rest, body_ty, body);

                (
                    core::Tm::TyFunLit {
                        name: name.clone(),
                        tm: Rc::new(body),
                    },
                    core::Ty::TyFunType {
                        name: name.clone(),
                        ty: Rc::new(body_ty),
                    },
                )
            }
            ([Param::Param { binder: name, ty }, rest @ ..], body_ty) => match ty {
                Some(param_ty) => {
                    let param_ty = elab_ty(ctx, param_ty);
                    let param_vty = eval_ty(ctx.ty_env.clone(), param_ty.clone());
                    let (body, body_ty) = go(&ctx.extend_tm(name, &param_vty), rest, body_ty, body);

                    (
                        core::Tm::FunLit {
                            name: name.clone(),
                            ty: param_ty.clone(),
                            tm: Rc::new(body),
                        },
                        core::Ty::FunType {
                            ty0: Rc::new(param_ty),
                            ty1: Rc::new(body_ty),
                        },
                    )
                }
                None => panic!("ambiguous parameter type"),
            },
        }
    }

    let (body, body_ty) = go(ctx, params, body_ty, body);

    (body, eval_ty(ctx.ty_env.clone(), body_ty))
}

pub mod core {
    use std::rc::Rc;

    use super::Env;

    // De Bruijn index that represents a variable occurrence by the number of
    // binders between the occurrence and the binder it refers to.
    pub type Index = usize;

    // De Bruijn level that represents a variable occurrence by the number of
    // binders from the top of the environment to the binder that the
    // occurrence refers to. These do not change their meaning as new bindings
    // are added to the environment.
    pub type Level = usize;

    // Converts a level to an index that is bound in an environment
    fn level_to_index(size: usize, level: Level) -> Index {
        size - level - 1
    }

    #[derive(Clone)]
    pub enum Ty {
        Var(Index),
        TyFunType { name: String, ty: Rc<Ty> },
        FunType { ty0: Rc<Ty>, ty1: Rc<Ty> },
        IntType,
        BoolType,
    }

    #[derive(Clone)]
    pub enum Tm {
        Var(Index),
        Let {
            name: String,
            ty: Ty,
            tm0: Rc<Tm>,
            tm1: Rc<Tm>,
        },
        TyFunLit {
            name: String,
            tm: Rc<Tm>,
        },
        TyFunApp {
            tm: Rc<Tm>,
            ty: Ty,
        },
        FunLit {
            name: String,
            ty: Ty,
            tm: Rc<Tm>,
        },
        FunApp {
            tm0: Rc<Tm>,
            tm1: Rc<Tm>,
        },
        IntLit(i32),
        BoolLit(bool),
        BoolElim {
            guard: Rc<Tm>,
            tm0: Rc<Tm>,
            tm1: Rc<Tm>,
        },
        PrimApp {
            prim: semantics::prim::Prim,
            tms: Vec<Tm>,
        },
    }

    // Pretty printing

    pub fn pp_ty(ty_names: &Env<String>, f: std::fmt::Formatter, ty: Ty) {
        todo!()
    }

    pub mod semantics {
        use std::rc::Rc;

        use crate::system_f_bidi::Env;

        use super::{level_to_index, Level, Tm, Ty};

        #[derive(Clone)]
        pub enum Vty {
            Neu(Nty),
            TyFunType {
                name: String,
                f: Rc<dyn Fn(&Vty) -> Vty>,
            },
            FunType {
                ty0: Rc<Vty>,
                ty1: Rc<Vty>,
            },
            IntType,
            BoolType,
        }

        // Neutral type values.
        //
        // We don't have eliminators in types in SYstem-F, so type neutrals only
        // consist of variables. This would be extended with type function
        // applications when moving to System-FOmega
        #[derive(Clone, Debug)]
        pub enum Nty {
            Var(Level),
        }

        #[derive(Clone)]
        pub enum Vtm {
            Neu(Ntm),
            TyFunLit {
                name: String,
                f: Rc<dyn Fn(&Vty) -> Vtm>,
            },
            FunLit {
                name: String,
                ty: Vty,
                f: Rc<dyn Fn(&Vtm) -> Vtm>,
            },
            IntLit(i32),
            BoolLit(bool),
        }

        #[derive(Clone)]
        pub enum Ntm {
            Var(Level),
            TyFunApp {
                tm: Rc<Ntm>,
                ty: Vty,
            },
            FunApp {
                ntm: Rc<Ntm>,
                vtm: Rc<Vtm>,
            },
            BoolElim {
                ntm: Rc<Ntm>,
                vtm0: Rc<Vtm>,
                vtm1: Rc<Vtm>,
            },
            PrimApp {
                prim: prim::Prim,
                args: Vec<Vtm>,
            },
        }

        // Eliminators

        fn ty_fun_app(head: &Vtm, arg: &Vty) -> Vtm {
            match head {
                Vtm::Neu(ntm) => Vtm::Neu(Ntm::TyFunApp {
                    tm: Rc::new(ntm.clone()),
                    ty: arg.clone(),
                }),
                Vtm::TyFunLit { f, .. } => f(arg),
                _ => panic!("expected type function!"),
            }
        }

        fn fun_app(head: &Vtm, arg: &Vtm) -> Vtm {
            match head {
                Vtm::Neu(ntm) => Vtm::Neu(Ntm::FunApp {
                    ntm: Rc::new(ntm.clone()),
                    vtm: Rc::new(arg.clone()),
                }),
                Vtm::FunLit { f, .. } => f(arg),
                _ => panic!("expected function!"),
            }
        }

        fn bool_elim(head: &Vtm, vtm0: &Vtm, vtm1: &Vtm) -> Vtm {
            match head {
                Vtm::Neu(ntm) => Vtm::Neu(Ntm::BoolElim {
                    ntm: Rc::new(ntm.clone()),
                    vtm0: Rc::new(vtm0.clone()),
                    vtm1: Rc::new(vtm1.clone()),
                }),
                Vtm::BoolLit(b) => match b {
                    true => vtm0.clone(),
                    false => vtm1.clone(),
                },
                _ => panic!("expected boolean!"),
            }
        }

        fn prim_app(prim: &prim::Prim) -> Rc<dyn Fn(&[Vtm]) -> Vtm> {
            match prim {
                prim::Prim::BoolEq => Rc::new(|args: &[Vtm]| match args {
                    [Vtm::BoolLit(b1), Vtm::BoolLit(b2)] => Vtm::BoolLit(b1 == b2),
                    _ => panic!("bad argument types"),
                }),
                prim::Prim::IntEq => Rc::new(|args: &[Vtm]| match args {
                    [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::BoolLit(i1 == i2),
                    _ => panic!("bad argument types"),
                }),
                prim::Prim::IntAdd => Rc::new(|args: &[Vtm]| match args {
                    [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 + i2),
                    _ => panic!("bad argument types"),
                }),
                prim::Prim::IntSub => Rc::new(|args: &[Vtm]| match args {
                    [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 - i2),
                    _ => panic!("bad argument types"),
                }),
                prim::Prim::IntMul => Rc::new(|args: &[Vtm]| match args {
                    [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 * i2),
                    _ => panic!("bad argument types"),
                }),
                prim::Prim::IntNeg => Rc::new(|args: &[Vtm]| match args {
                    [Vtm::IntLit(i1)] => Vtm::IntLit(-i1),
                    _ => panic!("bad argument types"),
                }),
            }
        }

        // Evaluation

        // Evaluate a type from the syntax into its semantic interpretation
        pub fn eval_ty(ty_env: Env<Vty>, ty: Ty) -> Vty {
            match ty {
                Ty::Var(index) => ty_env.get(index).clone(),
                Ty::TyFunType { name, ty: body_ty } => Vty::TyFunType {
                    name: name.clone(),
                    f: Rc::new(move |arg| {
                        eval_ty(ty_env.with(arg.clone()), body_ty.as_ref().clone())
                    }),
                },
                Ty::FunType { ty0, ty1 } => {
                    let vty0 = eval_ty(ty_env.clone(), ty0.as_ref().clone());
                    let vty1 = eval_ty(ty_env, ty1.as_ref().clone());

                    Vty::FunType {
                        ty0: Rc::new(vty0),
                        ty1: Rc::new(vty1),
                    }
                }
                Ty::IntType => Vty::IntType,
                Ty::BoolType => Vty::BoolType,
            }
        }

        // Evaluate a term from the syntax into its semantic interpretation
        fn eval_tm(ty_env: Env<Vty>, tm_env: Env<Vtm>, tm: Tm) -> Vtm {
            match tm {
                Tm::Var(index) => tm_env.get(index).clone(),
                Tm::Let { tm0, tm1, name, ty } => {
                    let vtm0 = eval_tm(ty_env.clone(), tm_env.clone(), tm0.as_ref().clone());
                    eval_tm(ty_env, tm_env.with(vtm0), tm1.as_ref().clone())
                }
                Tm::TyFunLit { name, tm } => Vtm::TyFunLit {
                    name,
                    f: Rc::new(move |arg| {
                        eval_tm(
                            ty_env.with(arg.clone()),
                            tm_env.clone(),
                            tm.as_ref().clone(),
                        )
                    }),
                },
                Tm::TyFunApp { tm, ty } => {
                    let vtm = eval_tm(ty_env.clone(), tm_env, tm.as_ref().clone());
                    let vty = eval_ty(ty_env.clone(), ty);

                    ty_fun_app(&vtm, &vty)
                }
                Tm::FunLit { name, ty, tm } => {
                    let vty = eval_ty(ty_env.clone(), ty);

                    Vtm::FunLit {
                        name,
                        ty: vty,
                        f: Rc::new(move |arg| {
                            eval_tm(
                                ty_env.clone(),
                                tm_env.with(arg.clone()),
                                tm.as_ref().clone(),
                            )
                        }),
                    }
                }
                Tm::FunApp { tm0, tm1 } => {
                    let vtm0 = eval_tm(ty_env.clone(), tm_env.clone(), tm0.as_ref().clone());
                    let vtm1 = eval_tm(ty_env.clone(), tm_env.clone(), tm1.as_ref().clone());
                    fun_app(&vtm0, &vtm1)
                }
                Tm::IntLit(i) => Vtm::IntLit(i),
                Tm::BoolLit(b) => Vtm::BoolLit(b),
                Tm::BoolElim { guard, tm0, tm1 } => {
                    let vguard = eval_tm(ty_env.clone(), tm_env.clone(), guard.as_ref().clone());

                    let vtm0 = eval_tm(ty_env.clone(), tm_env.clone(), tm0.as_ref().clone());
                    let vtm1 = eval_tm(ty_env.clone(), tm_env.clone(), tm1.as_ref().clone());

                    bool_elim(&vguard, &vtm0, &vtm1)
                }
                Tm::PrimApp { prim, tms } => {
                    let f = prim_app(&prim);

                    let v = tms
                        .iter()
                        .map(|tm| eval_tm(ty_env.clone(), tm_env.clone(), tm.clone()))
                        .collect::<Vec<_>>();

                    f(&v[..])
                }
            }
        }

        // Quotation

        // Convert types from the semantic domain back into syntax.
        pub fn quote_vty(ty_size: usize, vty: &Vty) -> Ty {
            match vty {
                Vty::Neu(nty) => match nty {
                    Nty::Var(level) => Ty::Var(level_to_index(ty_size, *level)),
                },
                Vty::TyFunType { name, f } => {
                    let body = quote_vty(ty_size + 1, &f(&Vty::Neu(Nty::Var(ty_size))));

                    Ty::TyFunType {
                        name: name.clone(),
                        ty: Rc::new(body),
                    }
                }
                Vty::FunType { ty0, ty1 } => {
                    let param_ty = quote_vty(ty_size, ty0);
                    let body_ty = quote_vty(ty_size, ty1);
                    Ty::FunType {
                        ty0: Rc::new(param_ty),
                        ty1: Rc::new(body_ty),
                    }
                }
                Vty::IntType => Ty::IntType,
                Vty::BoolType => Ty::BoolType,
            }
        }

        // Convert terms from the semantic domain back into syntax
        fn quote_vtm(ty_size: usize, tm_size: usize, vtm: &Vtm) -> Tm {
            match vtm {
                Vtm::Neu(ntm) => quote_ntm(ty_size, tm_size, ntm.clone()),
                Vtm::TyFunLit { name, f } => {
                    let body = quote_vtm(ty_size + 1, tm_size, &f(&Vty::Neu(Nty::Var(ty_size))));
                    Tm::TyFunLit {
                        name: name.clone(),
                        tm: Rc::new(body),
                    }
                }
                Vtm::FunLit { name, ty, f } => {
                    let param_ty = quote_vty(ty_size, ty);
                    let body = quote_vtm(ty_size, tm_size + 1, &f(&Vtm::Neu(Ntm::Var(tm_size))));
                    Tm::FunLit {
                        name: name.clone(),
                        ty: param_ty,
                        tm: Rc::new(body),
                    }
                }
                Vtm::IntLit(i) => Tm::IntLit(*i),
                Vtm::BoolLit(b) => Tm::BoolLit(*b),
            }
        }

        fn quote_ntm(ty_size: usize, tm_size: usize, ntm: Ntm) -> Tm {
            match ntm {
                Ntm::Var(level) => Tm::Var(level_to_index(tm_size, level)),
                Ntm::TyFunApp { tm, ty } => Tm::TyFunApp {
                    tm: Rc::new(quote_ntm(ty_size, tm_size, tm.as_ref().clone())),
                    ty: quote_vty(ty_size, &ty),
                },
                Ntm::FunApp { ntm, vtm } => Tm::FunApp {
                    tm0: Rc::new(quote_ntm(ty_size, tm_size, ntm.as_ref().clone())),
                    tm1: Rc::new(quote_vtm(ty_size, tm_size, &vtm)),
                },
                Ntm::BoolElim { ntm, vtm0, vtm1 } => {
                    let tm0 = quote_vtm(ty_size, tm_size, &vtm0);
                    let tm1 = quote_vtm(ty_size, tm_size, &vtm1);
                    Tm::BoolElim {
                        guard: Rc::new(quote_ntm(ty_size, tm_size, ntm.as_ref().clone())),
                        tm0: Rc::new(tm0),
                        tm1: Rc::new(tm1),
                    }
                }
                Ntm::PrimApp { prim, args } => Tm::PrimApp {
                    prim,
                    tms: args
                        .iter()
                        .map(|vtm| quote_vtm(ty_size, tm_size, vtm))
                        .collect(),
                },
            }
        }

        pub mod prim {
            #[derive(Clone)]
            pub enum Prim {
                BoolEq,
                IntEq,

                IntAdd,
                IntSub,
                IntMul,

                IntNeg,
            }
        }
    }
}
