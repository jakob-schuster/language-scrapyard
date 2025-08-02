use std::fmt::Display;
use std::{collections::HashMap, rc::Rc};

use crate::util::Env;
use crate::{core, prim};

// the start and end position in a source file
#[derive(Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("[{}-{}]", self.start, self.end).fmt(f)
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

// spanned nodes
#[derive(Clone)]
pub struct Spanned<T> {
    span: Span,
    data: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, data: T) -> Spanned<T> {
        Spanned { span, data }
    }
}

// types in the surface language
pub type Ty = Spanned<TyData>;
#[derive(Clone)]
pub enum TyData {
    Name { name: String },
    FunTy { head_ty: Rc<Ty>, body_ty: Rc<Ty> },
}

// names that bind definitions or parameters
type Binder = Spanned<String>;

// terms in the surface language
pub type Tm = Spanned<TmData>;
#[derive(Clone)]
pub enum TmData {
    Name {
        name: String,
    },
    Let {
        def_name: Binder,
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
        body: Rc<Tm>,
    },
    BoolLit {
        b: bool,
    },
    IntLit {
        i: i32,
    },
    App {
        head: Rc<Tm>,
        arg: Rc<Tm>,
    },
    IfThenElse {
        head: Rc<Tm>,
        tm1: Rc<Tm>,
        tm2: Rc<Tm>,
    },
    Infix {
        op: InfixOp,
        tm1: Rc<Tm>,
        tm2: Rc<Tm>,
    },
    Prefix {
        op: PrefixOp,
        tm: Rc<Tm>,
    },
}

// parameters, with optional type annotations
#[derive(Clone)]
pub struct Param {
    pub binder: Binder,
    pub ty: Option<Ty>,
}

// infix operators
#[derive(Clone)]
pub enum InfixOp {
    Eq,
    Add,
    Sub,
    Mul,
}

// prefix operators
#[derive(Clone)]
pub enum PrefixOp {
    Neg,
}

// error message that should be reported to the programmer
#[derive(Clone)]
pub struct Error {
    span: Span,
    message: String,
}

impl Error {
    fn new(span: &Span, message: &str) -> Error {
        Error {
            span: span.clone(),
            message: String::from(message),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("{} {}", self.span, self.message).fmt(f)
    }
}

// the elaboration context
#[derive(Clone)]
pub struct Context {
    // a stack of bindings currently in scope
    pub tys: HashMap<String, (core::Index, core::Ty)>,

    // erorr messages recorded during elaboration
    pub errors: Vec<Error>,
}

impl Context {
    // the empty context
    fn empty() -> Context {
        Context {
            tys: HashMap::new(),
            errors: Vec::new(),
        }
    }

    // extend the context with a new binding
    fn extend(&self, name: &str, ty: &core::Ty) -> Context {
        let mut tys = self.tys.clone();
        tys.insert(name.to_string(), (self.tys.len(), ty.clone()));

        Context {
            tys,
            errors: self.errors.clone(),
        }
    }

    // lookup a name in the context
    fn lookup(&self, name: &str) -> Option<(core::Index, core::Ty)> {
        self.tys.get(name).cloned()
    }

    // record an error in the elaboration context
    fn report(&self, error: Error) -> Context {
        let mut errors = self.errors.clone();
        errors.push(error);

        Context {
            tys: self.tys.clone(),
            errors,
        }
    }
}

// check if two types are compatible with each other
fn equate_tys(span: &Span, ty1: &core::Ty, ty2: &core::Ty) -> Result<(), Error> {
    match core::equate_tys(ty1, ty2) {
        true => Ok(()),
        false => Err(Error::new(
            span,
            &format!("mismatched types; {} and {}", ty1, ty2),
        )),
    }
}

// elaborate a type, checking that it is well-formed
fn check_ty(ctx: &Context, ty: &Ty) -> (core::Ty, Context) {
    match &ty.data {
        TyData::Name { name } => match name.as_str() {
            "Bool" => (core::Ty::BoolTy, ctx.clone()),
            "Int" => (core::Ty::IntTy, ctx.clone()),
            _ => (
                core::Ty::UnknownTy,
                ctx.report(Error::new(&ty.span, &format!("unbound name {}", name))),
            ),
        },
        TyData::FunTy { head_ty, body_ty } => {
            let (head_ty, ctx1) = check_ty(ctx, head_ty);
            let (body_ty, ctx2) = check_ty(&ctx1, body_ty);

            (
                core::Ty::FunTy {
                    head: Rc::new(head_ty),
                    body: Rc::new(body_ty),
                },
                ctx2.clone(),
            )
        }
    }
}

// elaborate a surface term into a core term, given an expected type
fn check_tm(ctx: &Context, tm: &Tm, ty: &core::Ty) -> (core::Tm, Context) {
    match &tm.data {
        TmData::Let {
            def_name,
            params,
            def_body_ty,
            def_body,
            body,
        } => {
            let (def, def_ty, ctx1) = infer_fun_lit(ctx, params, def_body_ty, def_body);
            let (body, ctx2) = check_tm(&ctx1.extend(&def_name.data, &def_ty), body, ty);

            (
                core::Tm::Let {
                    def_name: def_name.data.clone(),
                    def_ty,
                    def: Rc::new(def),
                    body: Rc::new(body),
                },
                ctx2,
            )
        }
        TmData::FunLit { params, body } => check_fun_lit(ctx, params, body, ty),
        TmData::IfThenElse { head, tm1, tm2 } => {
            let (core_head, ctx1) = check_tm(ctx, head, &core::Ty::BoolTy);
            let (core_tm1, ctx2) = check_tm(&ctx1, tm1, ty);
            let (core_tm2, ctx3) = check_tm(&ctx2, tm2, ty);

            (
                core::Tm::BoolElim {
                    head: Rc::new(core_head),
                    tm1: Rc::new(core_tm1),
                    tm2: Rc::new(core_tm2),
                },
                ctx3.clone(),
            )
        }

        // fall back to type inference
        _ => {
            let (core_tm, core_ty, ctx1) = infer_tm(ctx, tm);

            match equate_tys(&tm.span, ty, &core_ty) {
                Ok(()) => (core_tm, ctx1),
                Err(error) => (core::Tm::ReportedError, ctx1.report(error)),
            }
        }
    }
}

fn infer_tm(ctx: &Context, tm: &Tm) -> (core::Tm, core::Ty, Context) {
    match &tm.data {
        TmData::Name { name } => match ctx.lookup(name) {
            Some((index, ty)) => (core::Tm::Var { index }, ty, ctx.clone()),
            None => match name.as_str() {
                "true" => (core::Tm::BoolLit { b: true }, core::Ty::BoolTy, ctx.clone()),
                "false" => (core::Tm::BoolLit { b: true }, core::Ty::BoolTy, ctx.clone()),
                _ => (
                    core::Tm::ReportedError,
                    core::Ty::UnknownTy,
                    ctx.report(Error::new(&tm.span, &format!("unbound name '{}'", name))),
                ),
            },
        },
        TmData::Let {
            def_name,
            params,
            def_body_ty,
            def_body,
            body,
        } => {
            let (core_def, core_def_ty, ctx1) = infer_fun_lit(ctx, params, def_body_ty, def_body);
            let (core_body, core_body_ty, ctx2) =
                infer_tm(&ctx1.extend(&def_name.data, &core_def_ty), body);

            (
                core::Tm::Let {
                    def_name: def_name.data.clone(),
                    def_ty: core_def_ty,
                    def: Rc::new(core_def),
                    body: Rc::new(core_body),
                },
                core_body_ty,
                ctx2,
            )
        }
        TmData::Ann { tm, ty } => {
            let (core_ty, ctx1) = check_ty(ctx, ty);
            let (core_tm, ctx2) = check_tm(&ctx1, tm, &core_ty);

            (core_tm, core_ty, ctx2)
        }
        TmData::FunLit { params, body } => infer_fun_lit(ctx, params, &None, body),
        TmData::BoolLit { b } => (core::Tm::BoolLit { b: *b }, core::Ty::BoolTy, ctx.clone()),
        TmData::IntLit { i } => (core::Tm::IntLit { i: *i }, core::Ty::IntTy, ctx.clone()),
        TmData::App { head, arg } => {
            let (core_head, core_head_ty, ctx1) = infer_tm(ctx, head);
            match core_head_ty {
                core::Ty::FunTy {
                    head: head_ty,
                    body,
                } => {
                    let (core_arg, ctx2) = check_tm(&ctx1, arg, &head_ty);

                    (
                        core::Tm::FunApp {
                            head: Rc::new(core_head),
                            arg: Rc::new(core_arg),
                        },
                        body.as_ref().clone(),
                        ctx2,
                    )
                }
                core::Ty::UnknownTy => (core::Tm::ReportedError, core::Ty::UnknownTy, ctx.clone()),
                _ => (
                    core::Tm::ReportedError,
                    core::Ty::UnknownTy,
                    ctx.report(Error::new(&tm.span, "unexpected argument")),
                ),
            }
        }
        TmData::IfThenElse { head, tm1, tm2 } => {
            let (core_head, ctx1) = check_tm(ctx, head, &core::Ty::BoolTy);
            let (core_tm1, core_ty1, ctx2) = infer_tm(&ctx1, tm1);
            let (core_tm2, core_ty2, ctx3) = infer_tm(&ctx2, tm2);

            match core::meet_tys(&core_ty1, &core_ty2) {
                Some(core_ty) => (
                    core::Tm::BoolElim {
                        head: Rc::new(core_head),
                        tm1: Rc::new(core_tm1),
                        tm2: Rc::new(core_tm2),
                    },
                    core_ty,
                    ctx3,
                ),
                None => (
                    core::Tm::ReportedError,
                    core::Ty::UnknownTy,
                    ctx3.report(Error::new(&tm.span, "mismatched branches of if expression")),
                ),
            }
        }
        TmData::Infix { op, tm1, tm2 } => match op {
            InfixOp::Eq => {
                let (core_tm1, core_ty1, ctx1) = infer_tm(ctx, tm1);
                let (core_tm2, core_ty2, ctx2) = infer_tm(&ctx1, tm2);
                match core::meet_tys(&core_ty1, &core_ty2) {
                    Some(core_ty) => match core_ty {
                        core::Ty::UnknownTy => (core::Tm::ReportedError, core::Ty::UnknownTy, ctx2),
                        core::Ty::BoolTy => (
                            core::Tm::PrimApp {
                                prim: prim::Prim::BoolEq,
                                args: vec![core_tm1, core_tm2],
                            },
                            core::Ty::BoolTy,
                            ctx2,
                        ),
                        core::Ty::IntTy => (
                            core::Tm::PrimApp {
                                prim: prim::Prim::IntEq,
                                args: vec![core_tm1, core_tm2],
                            },
                            core::Ty::BoolTy,
                            ctx2,
                        ),
                        _ => (
                            core::Tm::ReportedError,
                            core::Ty::UnknownTy,
                            ctx2.report(Error::new(
                                &tm.span,
                                &format!("cannot compare operands of type '{}'", core_ty),
                            )),
                        ),
                    },
                    None => (
                        core::Tm::ReportedError,
                        core::Ty::UnknownTy,
                        ctx2.report(Error::new(
                            &tm.span,
                            &format!("mismatched operands; '{}' and '{}'", core_ty1, core_ty2),
                        )),
                    ),
                }
            }
            InfixOp::Add | InfixOp::Sub | InfixOp::Mul => {
                let prim = match op {
                    InfixOp::Add => prim::Prim::IntAdd,
                    InfixOp::Sub => prim::Prim::IntSub,
                    InfixOp::Mul => prim::Prim::IntMul,
                    // impossible!
                    _ => panic!(),
                };

                let (core_tm1, ctx1) = check_tm(ctx, tm1, &core::Ty::IntTy);
                let (core_tm2, ctx2) = check_tm(&ctx1, tm1, &core::Ty::IntTy);

                (
                    core::Tm::PrimApp {
                        prim,
                        args: vec![core_tm1, core_tm2],
                    },
                    core::Ty::IntTy,
                    ctx2,
                )
            }
        },
        TmData::Prefix { op, tm } => match op {
            PrefixOp::Neg => {
                let (core_tm, ctx1) = check_tm(ctx, tm, &core::Ty::IntTy);
                (
                    core::Tm::PrimApp {
                        prim: prim::Prim::IntNeg,
                        args: vec![core_tm],
                    },
                    core::Ty::IntTy,
                    ctx1,
                )
            }
        },
    }
}

// Elaborate a function literal into a core term, given an expected type.
fn check_fun_lit(ctx: &Context, params: &[Param], body: &Tm, ty: &core::Ty) -> (core::Tm, Context) {
    // Extract the parameter and body type, if possible, propagating unknown
    // types as required. See section 2.1.4 of "Total Type Error Localization
    // and Recovery with Holes" by Zhao et. al. for more details.
    fn match_fun_ty(ty: &core::Ty) -> Option<(core::Ty, core::Ty)> {
        match ty {
            core::Ty::FunTy { head, body } => Some((head.as_ref().clone(), body.as_ref().clone())),
            core::Ty::UnknownTy => Some((core::Ty::UnknownTy, core::Ty::UnknownTy)),
            _ => None,
        }
    }

    match (params, match_fun_ty(ty)) {
        ([], _) => check_tm(ctx, body, ty),
        ([Param { binder, ty: None }, rest @ ..], Some((param_ty, body_ty))) => {
            let (core_body, ctx1) =
                check_fun_lit(&ctx.extend(&binder.data, &param_ty), rest, body, &body_ty);

            (
                core::Tm::FunLit {
                    name: binder.data.clone(),
                    param_ty,
                    body: Rc::new(core_body),
                },
                ctx1,
            )
        }
        (
            [
                Param {
                    binder,
                    ty: Some(param_ty),
                },
                rest @ ..,
            ],
            Some((expected_param_ty, body_ty)),
        ) => {
            let (core_param_ty, ctx1) = check_ty(ctx, param_ty);

            match equate_tys(&param_ty.span, &core_param_ty, &expected_param_ty) {
                Ok(()) => {
                    let (core_body, ctx2) = check_fun_lit(&ctx1, rest, body, &body_ty);

                    (
                        core::Tm::FunLit {
                            name: binder.data.clone(),
                            param_ty: core_param_ty,
                            body: Rc::new(core_body),
                        },
                        ctx2,
                    )
                }
                // The explicit parameter did not match the expected type. Continue
                // checking the body of the function regardless.
                Err(error) => {
                    let ctx2 = ctx1.report(error);
                    let (_, ctx3) = check_fun_lit(&ctx2, rest, body, ty);
                    (core::Tm::ReportedError, ctx3)
                }
            }
        }

        // If we see an unexpected parameter, we check the parameter type regardless
        // and continue checking the body of the function.
        (
            [
                Param {
                    binder,
                    ty: param_ty,
                },
                rest @ ..,
            ],
            None,
        ) => {
            let ctx1 = &ctx.report(Error::new(&binder.span, "unexpected parameter"));

            let (param_ty, ctx2) = match param_ty {
                Some(param_ty) => check_ty(ctx1, param_ty),
                None => (core::Ty::UnknownTy, ctx1.clone()),
            };

            let (_, ctx3) = check_fun_lit(
                &ctx2.extend(&binder.data, &param_ty),
                params,
                body,
                &core::Ty::UnknownTy,
            );

            (core::Tm::ReportedError, ctx3)
        }
    }
}

// Elaborate a function literal into a core term, inferring its type.
fn infer_fun_lit(
    ctx: &Context,
    params: &[Param],
    body_ty: &Option<Ty>,
    body: &Tm,
) -> (core::Tm, core::Ty, Context) {
    match (params, body_ty) {
        ([], Some(body_ty)) => {
            let (core_body_ty, ctx1) = check_ty(ctx, body_ty);
            let (core_body, ctx2) = check_tm(&ctx1, body, &core_body_ty);
            (core_body, core_body_ty, ctx2)
        }
        ([], None) => infer_tm(ctx, body),
        (
            [
                Param {
                    binder,
                    ty: param_ty,
                },
                rest @ ..,
            ],
            body_ty,
        ) => {
            let (core_param_ty, ctx1) = match param_ty {
                Some(ty) => check_ty(ctx, ty),
                None => (
                    core::Ty::UnknownTy,
                    ctx.report(Error::new(&binder.span, "ambiguous parameter type")),
                ),
            };

            let (core_body, core_body_ty, ctx2) = infer_fun_lit(
                &ctx1.extend(&binder.data, &core_param_ty),
                rest,
                body_ty,
                body,
            );

            (
                core::Tm::FunLit {
                    name: binder.data.clone(),
                    param_ty: core_param_ty.clone(),
                    body: Rc::new(core_body),
                },
                core::Ty::FunTy {
                    head: Rc::new(core_param_ty),
                    body: Rc::new(core_body_ty),
                },
                ctx2,
            )
        }
    }
}

// Running elaboration

struct ElabError {
    errors: Vec<Error>,
}

pub fn elab(tm: &Tm) -> (core::Tm, core::Ty, Context) {
    let ctx = Context::empty();

    let (core_tm, core_ty, ctx1) = infer_tm(&ctx, tm);

    (core_tm, core_ty, ctx1)
}
