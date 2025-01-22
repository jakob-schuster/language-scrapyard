use std::{collections::HashMap, rc::Rc};

use crate::core;

#[derive(Clone, Debug)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Location {
        Location { start, end }
    }
}

#[derive(Clone, Debug)]
pub struct Located<T> {
    location: Location,
    data: T,
}

impl<T> Located<T> {
    pub fn new(location: Location, data: T) -> Located<T> {
        Located { location, data }
    }
}

pub type Binder = Located<String>;

#[derive(Clone, Debug)]
pub struct Param {
    binder: Binder,
    ty: Option<Ty>,
}

impl Param {
    pub fn new(binder: Binder, ty: Option<Ty>) -> Param {
        Param { binder, ty }
    }
}

pub type Ty = Located<TyData>;
#[derive(Clone, Debug)]
pub enum TyData {
    // References to named things: [ x ]
    Name(String),

    // Function types: [ fun (x : A) -> B x ]
    FunType { ty1: Rc<Ty>, ty2: Rc<Ty> },
}

pub type Tm = Located<TmData>;
#[derive(Clone, Debug)]
pub enum TmData {
    // References to named things: [ x ]
    Name(String),

    // Let expressions: [ let x : A := t; f x ]
    // Note: this is like an OCaml GLOBAL let, not local let.
    // i.e. you're letting a function with parameters, which may just have no parameters
    // So really it's like a function definition NOT a let-in
    Let {
        /// The function name
        binder: Binder,

        /// The (optionally typed) parameters
        params: Vec<Param>,

        /// The type of the function itself
        ty: Option<Ty>,

        /// The body of the function
        tm0: Rc<Tm>,

        /// The rest of the program, after the global let
        tm1: Rc<Tm>,
    },

    // Terms annotated with types: [ x : A ]
    Ann {
        tm: Rc<Tm>,
        ty: Rc<Ty>,
    },

    // Function literals: [ fun x => f x ]
    FunLit {
        params: Vec<Param>,
        body: Rc<Tm>,
    },

    // Function application: [ f x ]
    App {
        fun: Rc<Tm>,
        arg: Rc<Tm>,
    },

    // Boolean literals: [ true ]
    BoolLit {
        b: bool,
    },

    // Integer literals: [ 1 ]
    IntLit {
        i: i32,
    },

    // Conditionals: [ if x then y else z ]
    IfThenElse {
        guard: Rc<Tm>,
        tm0: Rc<Tm>,
        tm1: Rc<Tm>,
    },

    // Binary operators: [ x + z ]
    Op2 {
        op: Op2,
        tm1: Rc<Tm>,
        tm2: Rc<Tm>,
    },

    // Unary operators: [ -x ]
    Op1 {
        op: Op1,
        tm: Rc<Tm>,
    },
}

#[derive(Clone, Debug)]
pub enum Op2 {
    Eq,
    Add,
    Sub,
    Mul,
    Mod,
}

#[derive(Clone, Debug)]
pub enum Op1 {
    Neg,
}

// A stack of bindings currently in scope
#[derive(Default)]
pub struct Context {
    map: HashMap<String, (usize, core::Ty)>,
}
impl Context {
    fn with(&self, name: Binder, t: core::Ty) -> Context {
        let mut new_map = self.map.clone();
        let index = self.map.len();

        new_map.insert(name.data, (index, t));

        Context { map: new_map }
    }

    fn get(&self, name: &String) -> Option<&(usize, core::Ty)> {
        self.map.get(name)
    }
}

// An error that will be raised if there was a problem in the surface syntax,
// usually as a result of type errors. This is normal, and should be rendered
// nicely to the programmer.
#[derive(Debug)]
pub struct ElabError {
    pub location: Location,
    pub message: String,
}

impl ElabError {
    fn new(location: &Location, message: &str) -> ElabError {
        ElabError {
            location: location.clone(),
            message: message.to_string(),
        }
    }
}

fn equate_ty(location: &Location, ty1: &core::Ty, ty2: &core::Ty) -> Result<(), ElabError> {
    if ty1 == ty2 {
        Ok(())
    } else {
        Err(ElabError::new(
            location,
            &format!("mismatched types: expected {}, found {}", ty2, ty1),
        ))
    }
}

fn elab_ty(ty: &Ty) -> Result<core::Ty, ElabError> {
    match &ty.data {
        TyData::Name(n) => match &n[..] {
            "Bool" => Ok(core::Ty::BoolType),
            "Int" => Ok(core::Ty::IntType),

            _ => Err(ElabError::new(
                &ty.location,
                &format!("unknown type: {}", n),
            )),
        },

        TyData::FunType { ty1, ty2 } => Ok(core::Ty::FunType {
            from: Rc::new(elab_ty(ty1)?),
            to: Rc::new(elab_ty(ty2)?),
        }),
    }
}

fn elab_check(ctx: &Context, tm: &Tm, ty: &core::Ty) -> Result<core::Tm, ElabError> {
    match &tm.data {
        TmData::Let {
            binder,
            params,
            ty: def_body_ty,
            tm0: def_body,
            tm1: body,
        } => {
            let (def_c, def_cty) =
                elab_infer_fun_lit(ctx, params, &def_body_ty.clone(), def_body, &tm.location)?;
            let body_c = elab_check(&ctx.with(binder.clone(), def_cty.clone()), body, ty)?;

            Ok(core::Tm::Let(
                binder.data.clone(),
                def_cty,
                Rc::new(def_c),
                Rc::new(body_c),
            ))
        }
        TmData::FunLit { params, body: tm } => {
            elab_check_fun_lit(ctx, params, tm, ty, &tm.location)
        }
        TmData::IfThenElse {
            guard,
            tm0: t1,
            tm1: t2,
        } => {
            let core_guard = elab_check(ctx, guard, &core::Ty::BoolType)?;
            let core_t1 = elab_check(ctx, t1, ty)?;
            let core_t2 = elab_check(ctx, t2, ty)?;

            Ok(core::Tm::BoolElim(
                Rc::new(core_guard),
                Rc::new(core_t1),
                Rc::new(core_t2),
            ))
        }

        // Fall back to type inference
        _ => {
            let (tm1, ty1) = elab_infer(ctx, tm)?;
            equate_ty(&tm.location, &ty.clone(), &ty1)?;

            Ok(tm1)
        }
    }
}

pub fn elab_infer(ctx: &Context, tm: &Tm) -> Result<(core::Tm, core::Ty), ElabError> {
    match &tm.data {
        TmData::Name(name) => match ctx.get(name) {
            Some((index, ty)) => Ok((core::Tm::Var(*index), ty.clone())),
            None => Err(ElabError::new(
                &tm.location,
                &format!("unbound name: {name}"),
            )),
        },
        TmData::Let {
            binder: def_name,
            params,
            ty: def_body_ty,
            tm0: def_body,
            tm1: body,
        } => {
            let (def_c, def_cty) =
                elab_infer_fun_lit(ctx, params, &def_body_ty.clone(), def_body, &tm.location)?;
            let (body_c, body_cty) =
                elab_infer(&ctx.with(def_name.clone(), def_cty.clone()), body)?;

            Ok((
                core::Tm::Let(
                    def_name.data.clone(),
                    def_cty,
                    Rc::new(def_c),
                    Rc::new(body_c),
                ),
                body_cty,
            ))
        }
        TmData::Ann { tm, ty } => {
            let core_ty = elab_ty(ty)?;
            Ok((elab_check(ctx, tm, &core_ty)?, core_ty))
        }
        TmData::FunLit { params, body: tm } => {
            elab_infer_fun_lit(ctx, params, &None, tm, &tm.location)
        }
        TmData::BoolLit { b } => Ok((core::Tm::BoolLit(*b), core::Ty::BoolType)),
        TmData::IntLit { i } => Ok((core::Tm::IntLit(*i), core::Ty::IntType)),
        TmData::App { fun, arg } => {
            let (head, head_ty) = elab_infer(ctx, fun)?;
            let (param_ty, body_ty) = match head_ty {
                core::Ty::FunType { from, to } => Ok((from, to)),
                _ => Err(ElabError::new(
                    &tm.location,
                    &format!("mismatched types: expected _ -> _, found {}", head_ty),
                )),
            }?;

            let arg = elab_check(ctx, arg, &param_ty)?;

            Ok((
                core::Tm::FunApp(Rc::new(head), Rc::new(arg)),
                (*body_ty).clone(),
            ))
        }
        TmData::IfThenElse { .. } => Err(ElabError::new(&tm.location, "ambiguous if expression")),
        TmData::Op2 {
            op,
            tm1: t1,
            tm2: t2,
        } => {
            let (ct1, cty1) = elab_infer(ctx, t1)?;
            let (ct2, cty2) = elab_infer(ctx, t2)?;
            equate_ty(&tm.location, &cty1, &cty2)?;

            let (prim, cty) = match op {
                Op2::Eq => match cty1 {
                    core::Ty::BoolType => Ok((core::prim::Prim::BoolEq, core::Ty::BoolType)),
                    core::Ty::IntType => Ok((core::prim::Prim::IntEq, core::Ty::BoolType)),

                    _ => Err(ElabError::new(
                        &tm.location,
                        &format!("unsupported type {:?}", cty1),
                    )),
                },
                Op2::Add => {
                    equate_ty(&tm.location, &cty1, &core::Ty::IntType)?;
                    Ok((core::prim::Prim::IntAdd, core::Ty::IntType))
                }
                Op2::Sub => {
                    equate_ty(&tm.location, &cty1, &core::Ty::IntType)?;
                    Ok((core::prim::Prim::IntSub, core::Ty::IntType))
                }
                Op2::Mul => {
                    equate_ty(&tm.location, &cty1, &core::Ty::IntType)?;
                    Ok((core::prim::Prim::IntMul, core::Ty::IntType))
                }
                Op2::Mod => {
                    equate_ty(&tm.location, &cty1, &core::Ty::IntType)?;
                    Ok((core::prim::Prim::IntMod, core::Ty::IntType))
                }
            }?;

            Ok((core::Tm::PrimApp(prim, vec![ct1, ct2]), cty))
        }
        TmData::Op1 { op, tm: t } => {
            let (ct, cty1) = elab_infer(ctx, t)?;

            let (prim, cty) = match op {
                Op1::Neg => {
                    assert_eq!(cty1, core::Ty::IntType);
                    (core::prim::Prim::IntNeg, core::Ty::IntType)
                }
            };

            Ok((core::Tm::PrimApp(prim, vec![ct]), cty))
        }
    }
}

fn elab_check_fun_lit(
    ctx: &Context,
    params: &[Param],
    body: &Tm,
    ty: &core::Ty,
    loc: &Location,
) -> Result<core::Tm, ElabError> {
    match (params, ty) {
        ([], ty) => elab_check(ctx, body, ty),
        (
            [Param {
                binder: name,
                ty: None,
            }, rest @ ..],
            core::Ty::FunType { from, to },
        ) => {
            let body_c = elab_check_fun_lit(
                &ctx.with(name.clone(), from.as_ref().clone()),
                rest,
                body,
                to,
                loc,
            )?;

            Ok(core::Tm::FunLit(
                name.data.clone(),
                from.as_ref().clone(),
                Rc::new(body_c),
            ))
        }
        (
            [Param {
                binder: name,
                ty: Some(param_ty),
            }, rest @ ..],
            core::Ty::FunType { from, to },
        ) => {
            let param_cty = elab_ty(param_ty)?;

            // verify that the parameter has been assigned a type which makes sense for this function
            equate_ty(loc, &param_cty, from)?;

            let body_c = elab_check_fun_lit(
                &ctx.with(name.clone(), from.as_ref().clone()),
                rest,
                body,
                to,
                loc,
            )?;

            Ok(core::Tm::FunLit(
                name.data.clone(),
                param_cty,
                Rc::new(body_c),
            ))
        }

        _ => Err(ElabError::new(loc, "unexpected parameter")),
    }
}

fn elab_infer_fun_lit(
    ctx: &Context,
    params: &[Param],
    body_ty: &Option<Ty>,
    body: &Tm,
    loc: &Location,
) -> Result<(core::Tm, core::Ty), ElabError> {
    match (params, body_ty) {
        ([], Some(body_ty)) => {
            let body_cty = elab_ty(body_ty)?;

            Ok((elab_check(ctx, body, &body_cty)?, body_cty))
        }
        ([], None) => Ok(elab_infer(ctx, body)?),
        (
            [Param {
                binder: name,
                ty: None,
            }, rest @ ..],
            _,
        ) => Err(ElabError::new(loc, "ambiguous parameter type")),
        (
            [Param {
                binder: name,
                ty: Some(param_ty),
            }, rest @ ..],
            body_cty1,
        ) => {
            let param_cty = elab_ty(param_ty)?;

            let (body_c, body_cty) = elab_infer_fun_lit(
                &ctx.with(name.clone(), param_cty.clone()),
                rest,
                body_cty1,
                body,
                loc,
            )?;

            Ok((
                core::Tm::FunLit(name.data.clone(), param_cty.clone(), Rc::new(body_c)),
                core::Ty::FunType {
                    from: Rc::new(param_cty),
                    to: Rc::new(body_cty),
                },
            ))
        }
    }
}
