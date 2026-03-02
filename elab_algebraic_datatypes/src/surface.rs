use crate::core;
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
        tm: Rc<Tm>,
        rest: Rc<Tm>,
    },

    FunTy {
        args: Vec<Tm>,
        body: Rc<Tm>,
    },
    Fun {
        args: Vec<Tm>,
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
    pub tys: HashMap<String, (core::Index, core::Tm)>,

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

    // extend the context with a new binding
    fn extend(&self, name: &str, ty: &core::Tm) -> Context {
        let mut tys = self.tys.clone();
        tys.insert(name.to_string(), (self.tys.len(), ty.clone()));

        Context {
            tys,
            errors: self.errors.clone(),
        }
    }

    // lookup a name in the context
    fn lookup(&self, name: &str) -> Option<(core::Index, core::Tm)> {
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

fn check(ctx: &Context, tm: &Tm, ty: &core::Tm) -> (core::Tm, Context) {
    todo!()
}

// elaborate a term, checking that it is well-formed
fn infer(ctx: &Context, tm: &Tm) -> (core::Tm, core::Tm, Context) {
    match &tm.data {
        TmData::Named { name } => match name.as_str() {
            // look up name
            _ => (
                core::Tm::Error,
                core::Tm::ErrorTy,
                ctx.report(Error::new(&tm.span, &format!("unbound name {}", name))),
            ),
        },
        TmData::BoolTy => (core::Tm::BoolTy, core::Tm::UnivTy, ctx.clone()),
        TmData::Bool { b } => (core::Tm::Bool { b: *b }, core::Tm::BoolTy, ctx.clone()),
        TmData::NumTy => (core::Tm::NumTy, core::Tm::UnivTy, ctx.clone()),
        TmData::Num { n } => (core::Tm::Num { n: *n }, core::Tm::NumTy, ctx.clone()),
        TmData::EnumTy { variants } => {
            let mut ctx1 = ctx.clone();
            let mut core_variants: HashMap<String, HashMap<String, core::Tm>> = HashMap::new();
            for (name, fields) in variants {
                let mut core_fields: HashMap<String, core::Tm> = HashMap::new();
                for (name, field) in fields {
                    let (core_field, ctx2) = check(ctx, &field, &core::Tm::UnivTy);
                    ctx1 = ctx2;
                    core_fields.insert(name.clone(), core_field);
                }
                core_variants.insert(name.clone(), core_fields);
            }

            (
                core::Tm::EnumTy {
                    variants: core_variants,
                },
                core::Tm::UnivTy,
                ctx1,
            )
        }
        TmData::Enum { variant, fields } => todo!(),
        TmData::StructTy { fields } => todo!(),
        TmData::Struct { fields } => todo!(),
        TmData::Let { tm, rest } => todo!(),
        TmData::FunTy { args, body } => todo!(),
        TmData::Fun {
            args,
            body_ty,
            body,
        } => todo!(),
        TmData::FunApp { head, args } => todo!(),
        TmData::IfThenElse { guard, tm1, tm2 } => todo!(),
        TmData::Infix { op, tm1, tm2 } => todo!(),
        TmData::Prefix { op, tm } => todo!(),
    }
}
