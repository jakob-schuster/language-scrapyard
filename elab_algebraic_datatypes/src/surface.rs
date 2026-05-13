use crate::{
    core::{self, Env},
    prim,
};
use std::{collections::HashMap, fmt::Display, rc::Rc};

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

type Tm = Spanned<TmData>;
#[derive(Clone)]
enum TmData {
    Named {
        name: String,
    },

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
        variants: HashMap<String, HashMap<String, Tm>>,
    },
    Enum {
        head: Rc<Tm>,
        variant: String,
        fields: HashMap<String, Tm>,
    },

    StructTy {
        fields: HashMap<String, Tm>,
    },
    Struct {
        fields: HashMap<String, Tm>,
    },

    Let {
        name: String,
        tm: Rc<Tm>,
        rest: Rc<Tm>,
    },

    FunTy {
        args: Vec<Tm>,
        body: Rc<Tm>,
    },
    Fun {
        params: Vec<Param>,
        body_ty: Option<Rc<Tm>>,
        body: Rc<Tm>,
    },
    FunApp {
        head: Rc<Tm>,
        args: Vec<Tm>,
    },

    IfThenElse {
        guard: Rc<Tm>,
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

// a parameter of a function
#[derive(Clone)]
pub struct Param {
    name: String,
    ty: Rc<Tm>,
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
    pub tys: HashMap<String, (core::Index, core::Val, core::Val)>,

    // error messages recorded during elaboration
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

    // returns the next variable that will be bound in the context
    fn next_var(&self) -> core::Val {
        core::Val::Neutral {
            neutral: core::Neutral::Var { i: self.tys.len() },
        }
    }

    // binds a parameter in the context
    fn with_param(&self, name: &str, ty: &core::Val) -> Context {
        self.with(name, ty, &self.next_var())
    }

    // binds a definition in the context
    fn with(&self, name: &str, ty: &core::Val, val: &core::Val) -> Context {
        let mut tys = self.tys.clone();
        tys.insert(name.to_string(), (self.tys.len(), ty.clone(), val.clone()));

        Context {
            tys,
            errors: self.errors.clone(),
        }
    }

    // lookup a name in the context
    fn lookup(&self, name: &str) -> Option<(core::Index, core::Val, core::Val)> {
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

fn check(ctx: &Context, tm: &Tm, ty: &core::Val) -> (core::Tm, Context) {
    let (core_tm, core_tm_ty, ctx1) = infer(ctx, tm);

    if ty.equiv_ty(&core_tm_ty) {
        (core_tm, ctx1)
    } else {
        (
            core::Tm::Error,
            ctx.report(Error::new(
                &tm.span,
                &format!("mismatched types: expected {}, found {}", ty, core_tm_ty),
            )),
        )
    }
}

// elaborate a term, checking that it is well-formed
fn infer(ctx: &Context, tm: &Tm) -> (core::Tm, core::Val, Context) {
    match &tm.data {
        TmData::Named { name } => match ctx.lookup(name.as_str()) {
            Some((i, ty, _)) => (core::Tm::Var { i }, ty, ctx.clone()),

            // if the name was undefined, record error
            None => (
                core::Tm::Error,
                core::Val::ErrorTy,
                ctx.report(Error::new(&tm.span, &format!("unbound name {}", name))),
            ),
        },
        TmData::BoolTy => (core::Tm::BoolTy, core::Val::UnivTy, ctx.clone()),
        TmData::Bool { b } => (core::Tm::Bool { b: *b }, core::Val::BoolTy, ctx.clone()),
        TmData::NumTy => (core::Tm::NumTy, core::Val::UnivTy, ctx.clone()),
        TmData::Num { n } => (core::Tm::Num { n: *n }, core::Val::NumTy, ctx.clone()),
        TmData::EnumTy { variants } => {
            let mut ctx1 = ctx.clone();
            let mut core_variants: HashMap<String, HashMap<String, core::Tm>> = HashMap::new();
            for (name, fields) in variants {
                let mut core_fields: HashMap<String, core::Tm> = HashMap::new();
                for (name, field) in fields {
                    let (core_field, ctx2) = check(&ctx1, &field, &core::Val::UnivTy);
                    ctx1 = ctx2;
                    core_fields.insert(name.clone(), core_field);
                }
                core_variants.insert(name.clone(), core_fields);
            }

            (
                core::Tm::EnumTy {
                    variants: core_variants,
                },
                core::Val::UnivTy,
                ctx1,
            )
        }
        TmData::Enum {
            head,
            variant,
            fields,
        } => {
            let (core_head, ctx1) = check(ctx, head, &core::Val::UnivTy);
            let core_head_val = core_head.eval(&Env::from_ctx(ctx));
            let mut ctx2 = ctx1;

            let mut core_fields: HashMap<String, core::Tm> = HashMap::new();
            let mut core_field_tys: HashMap<String, core::Val> = HashMap::new();
            for (name, tm) in fields {
                let (core_tm, core_ty, ctx3) = infer(&ctx2, tm);
                ctx2 = ctx3;
                core_fields.insert(name.clone(), core_tm);
                core_field_tys.insert(name.clone(), core_ty);
            }

            match &core_head_val {
                core::Val::EnumTy { variants } => match variants.get(variant) {
                    Some(core_head_fields) => {
                        if core_field_tys.iter().eq(core_head_fields.iter()) {
                        } else {
                            ctx2 = ctx2.report(Error::new(
                                &tm.span,
                                &format!("expected a different set of fields when defining enum",),
                            ));
                        }
                    }
                    None => {
                        ctx2 = ctx2.report(Error::new(
                            &tm.span,
                            &format!("enum did not contain variant {}", variant),
                        ));
                    }
                },
                _ => {
                    ctx2 = ctx2.report(Error::new(
                        &tm.span,
                        &format!("tried to construct enum from a non-enum head type"),
                    ));
                }
            }

            (
                core::Tm::Enum {
                    head: Rc::new(core_head.clone()),
                    variant: variant.to_string(),
                    fields: core_fields,
                },
                core_head_val,
                ctx2,
            )
        }
        TmData::StructTy { fields } => {
            let mut ctx1 = ctx.clone();

            let mut core_fields = HashMap::new();
            for (name, field) in fields {
                let (core_field, ctx2) = check(&ctx1, field, &core::Val::UnivTy);
                ctx1 = ctx2;
                core_fields.insert(name.clone(), core_field);
            }

            (
                core::Tm::StructTy {
                    fields: core_fields,
                },
                core::Val::UnivTy,
                ctx1,
            )
        }
        TmData::Struct { fields } => {
            let mut ctx1 = ctx.clone();
            let mut core_fields = HashMap::new();
            let mut core_field_tys = HashMap::new();
            for (name, field) in fields {
                let (core_tm, core_ty, ctx2) = infer(&ctx1, field);
                ctx1 = ctx2;
                core_fields.insert(name.clone(), core_tm);
                core_field_tys.insert(name.clone(), core_ty);
            }

            (
                core::Tm::Struct {
                    fields: core_fields,
                },
                core::Val::StructTy {
                    fields: core_field_tys,
                },
                ctx1,
            )
        }
        TmData::Let { name, tm, rest } => {
            let (core_tm, core_ty, ctx1) = infer(ctx, tm);
            // the head needs to be evaluated, so it can be bound in the context
            let core_val = core_tm.eval(&Env::from_ctx(ctx));
            let ctx2 = ctx1.with(name, &core_ty, &core_val);

            let (rest_tm, rest_ty, ctx3) = infer(&ctx2, rest);

            (
                core::Tm::Let {
                    tm: Rc::new(core_tm),
                    rest: Rc::new(rest_tm),
                },
                rest_ty,
                ctx3,
            )
        }
        TmData::FunTy { args, body } => {
            let mut ctx1 = ctx.clone();
            let mut core_args = Vec::new();
            for arg in args {
                let (core_tm, ctx2) = check(&ctx1, arg, &core::Val::UnivTy);
                ctx1 = ctx2;
                core_args.push(core_tm);
            }

            let (core_body, ctx2) = check(&ctx1, body, &core::Val::UnivTy);

            (
                core::Tm::FunTy {
                    args: core_args,
                    body: Rc::new(core_body),
                },
                core::Val::UnivTy,
                ctx2,
            )
        }
        TmData::Fun {
            params,
            body_ty,
            body,
        } => {
            // either check or infer the return type of the function
            let (core_body, core_body_ty, ctx1) = match body_ty {
                Some(body_ty) => {
                    let (core_body_ty, ctx1) = check(ctx, body_ty, &core::Val::UnivTy);
                    // need to evaluate the body type in order to perform typechecking
                    let core_body_ty_val = core_body_ty.eval(&Env::from_ctx(ctx));
                    let (core_body, ctx2) = check(&ctx1, body, &core_body_ty_val);

                    (core_body, core_body_ty_val, ctx2)
                }
                None => infer(ctx, body),
            };

            let mut ctx2 = ctx1;
            let mut core_params = Vec::new();
            for param in params {
                let (core_param, ctx3) = check(&ctx2, &param.ty, &core::Val::UnivTy);
                ctx2 = ctx3;
                let core_param_val = core_param.eval(&Env::from_ctx(&ctx2));
                core_params.push(core_param_val);
            }

            (
                core::Tm::Fun {
                    body: Rc::new(core_body),
                },
                core::Val::FunTy {
                    args: core_params,
                    body: Rc::new(core_body_ty),
                },
                ctx2,
            )
        }
        TmData::FunApp { head, args } => {
            let (core_head, core_head_ty, ctx1) = infer(ctx, head);

            match core_head_ty {
                core::Val::FunTy {
                    args: core_head_ty_args,
                    body: core_head_ty_body,
                } => {
                    if core_head_ty_args.len().eq(&args.len()) {
                        let mut ctx2 = ctx1;
                        let mut core_args = Vec::new();
                        let mut core_arg_tys = Vec::new();

                        for (core_head_arg, arg) in core_head_ty_args.iter().zip(args) {
                            // check each argument is of the correct type
                            let (core_arg, ctx3) = check(&ctx2, arg, core_head_arg);
                            ctx2 = ctx3;
                            core_args.push(core_arg);
                            core_arg_tys.push(core_head_arg);
                        }

                        (
                            core::Tm::FunApp {
                                head: Rc::new(core_head),
                                args: core_args,
                            },
                            (*core_head_ty_body).clone(),
                            ctx2,
                        )
                    } else {
                        // unequal parameter vs argument list length
                        (
                            core::Tm::Error,
                            core::Val::ErrorTy,
                            ctx1.report(Error::new(
                                &tm.span,
                                "function was given the wrong number of arguments",
                            )),
                        )
                    }
                }
                // head was not a function
                _ => (
                    core::Tm::Error,
                    core::Val::ErrorTy,
                    ctx1.report(Error::new(
                        &tm.span,
                        "tried to apply something that was not a function",
                    )),
                ),
            }
        }
        TmData::IfThenElse { guard, tm1, tm2 } => {
            let (core_guard, ctx1) = check(ctx, guard, &core::Val::BoolTy);
            let (core_tm1, core_tm1_ty, ctx2) = infer(&ctx1, tm1);
            let (core_tm2, ctx3) = check(&ctx2, tm2, &core_tm1_ty);

            (
                core::Tm::IfThenElse {
                    guard: Rc::new(core_guard),
                    tm1: Rc::new(core_tm1),
                    tm2: Rc::new(core_tm2),
                },
                core_tm1_ty,
                ctx3,
            )
        }
        TmData::Infix { op, tm1, tm2 } => {
            // expect the two terms to match type!
            let (core_tm1, core_tm1_ty, ctx1) = infer(ctx, tm1);
            let (core_tm2, ctx2) = check(&ctx1, tm2, &core_tm1_ty);

            let (prim, ty, ctx3) = match op {
                InfixOp::Eq => {
                    match core_tm1_ty {
                        core::Val::BoolTy => (prim::Prim::BoolEq, core::Val::BoolTy, ctx2),
                        core::Val::NumTy => (prim::Prim::IntEq, core::Val::BoolTy, ctx2),
                        core::Val::EnumTy { variants: _ } => {
                            (prim::Prim::EnumEq, core::Val::BoolTy, ctx2)
                        }
                        core::Val::StructTy { fields: _ } => {
                            (prim::Prim::StructEq, core::Val::BoolTy, ctx2)
                        }
                        // type error!
                        _ => (
                            prim::Prim::Error,
                            core::Val::ErrorTy,
                            ctx2.report(Error::new(
                                &tm.span,
                                "tried to apply primitive operation to term of invalid type",
                            )),
                        ),
                    }
                }
                InfixOp::Add => match core_tm1_ty {
                    core::Val::NumTy => (prim::Prim::IntAdd, core::Val::NumTy, ctx2),
                    _ => (
                        prim::Prim::Error,
                        core::Val::ErrorTy,
                        ctx2.report(Error::new(
                            &tm.span,
                            "tried to apply primitive operation to term of invalid type",
                        )),
                    ),
                },
                InfixOp::Sub => match core_tm1_ty {
                    core::Val::NumTy => (prim::Prim::IntSub, core::Val::NumTy, ctx2),
                    _ => (
                        prim::Prim::Error,
                        core::Val::ErrorTy,
                        ctx2.report(Error::new(
                            &tm.span,
                            "tried to apply primitive operation to term of invalid type",
                        )),
                    ),
                },
                InfixOp::Mul => match core_tm1_ty {
                    core::Val::NumTy => (prim::Prim::IntMul, core::Val::NumTy, ctx2),
                    _ => (
                        prim::Prim::Error,
                        core::Val::ErrorTy,
                        ctx2.report(Error::new(
                            &tm.span,
                            "tried to apply primitive operation to term of invalid type",
                        )),
                    ),
                },
            };

            (
                core::Tm::PrimApp {
                    prim: prim,
                    args: vec![core_tm1, core_tm2],
                },
                ty,
                ctx3,
            )
        }
        TmData::Prefix { op, tm } => {
            let (core_tm, core_tm_ty, ctx1) = infer(ctx, tm);

            let (prim, ty, ctx2) = match op {
                PrefixOp::Neg => match core_tm_ty {
                    core::Val::NumTy => (prim::Prim::IntNeg, core::Val::NumTy, ctx1),
                    _ => (
                        prim::Prim::Error,
                        core::Val::ErrorTy,
                        ctx1.report(Error::new(
                            &tm.span,
                            "tried to apply primitive operation to term of invalid type",
                        )),
                    ),
                },
            };

            (
                core::Tm::PrimApp {
                    prim,
                    args: vec![core_tm],
                },
                ty,
                ctx2,
            )
        }
    }
}
