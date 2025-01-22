use crate::{
    core::{self, eval, EvalError},
    util::{self, Env, Located},
};
use std::rc::Rc;

pub type Tm = Located<TmData>;
pub type Ty = Tm;
pub enum TmData {
    Let {
        name: String,
        head: Rc<Tm>,
        body: Rc<Tm>,
    },
    Name {
        name: String,
    },

    BoolLit {
        b: bool,
    },
    IntLit {
        i: i32,
    },
    StrLit {
        s: String,
    },

    FunTy {
        args: Vec<Tm>,
        body: Rc<Tm>,
    },
    FunLit {
        args: Vec<Param>,
        ty: Rc<Ty>,
        body: Rc<Tm>,
    },
    FunApp {
        head: Rc<Tm>,
        args: Vec<Tm>,
    },
}

pub struct Param {
    pub name: String,
    pub ty: Ty,
}

/// An error that will be raised if there was a problem in the surface syntax,
/// usually as a result of type errors. This is normal, and should be rendered
/// nicely to the programmer.
#[derive(Debug)]
pub struct ElabError {
    pub location: util::Location,
    pub message: String,
}

impl ElabError {
    fn new(location: &util::Location, message: &str) -> ElabError {
        ElabError {
            location: location.clone(),
            message: message.to_string(),
        }
    }

    fn from_eval_error(eval_error: EvalError) -> ElabError {
        ElabError {
            location: eval_error.location,
            message: eval_error.message,
        }
    }
}

#[derive(Clone, Default)]
pub struct Context {
    size: usize,
    names: Env<String>,
    tys: Env<core::Vtm>,
    pub tms: Env<core::Vtm>,
}

impl Context {
    /// Returns the next variable that will be bound in the context after
    /// calling bind_def or bind_param
    fn next_var(&self) -> core::Vtm {
        core::Vtm::Ntm {
            ntm: core::Ntm::Var { level: self.size },
        }
    }

    /// Binds a definition in the context
    fn bind_def(&self, name: String, ty: core::Vty, tm: core::Vtm) -> Context {
        Context {
            size: self.size + 1,
            names: self.names.with(name),
            tys: self.tys.with(ty),
            tms: self.tms.with(tm),
        }
    }

    /// Binds a parameter in the context
    fn bind_param(&self, name: String, ty: core::Vty) -> Context {
        self.bind_def(name, ty, self.next_var())
    }

    /// Looks up a name in the context
    fn lookup(&self, name: String) -> Option<(usize, core::Vty)> {
        // Find the index of most recent binding in the context identified by
        // name, starting from the most recent binding. This gives us the
        // de Bruijn index of the variable.
        let index = self.names.find_last(&name)?;
        let vty = self.tys.get(index);

        Some((index, vty.clone()))
    }

    pub fn standard_library() -> Context {
        Context::default()
            .bind_def("Type".to_string(), core::Vty::Univ, core::Vty::Univ)
            .bind_def("Bool".to_string(), core::Vty::Univ, core::Vty::BoolTy)
            .bind_def("Int".to_string(), core::Vty::Univ, core::Vty::IntTy)
            .bind_def("Str".to_string(), core::Vty::Univ, core::Vty::StrTy)
    }
}

fn equate_ty(location: &util::Location, ty1: &core::Vty, ty2: &core::Vty) -> Result<(), ElabError> {
    if ty1.equiv(ty2) {
        Ok(())
    } else {
        Err(ElabError::new(
            location,
            &format!("mismatched types: expected {}, found {}", ty2, ty1),
        ))
    }
}

fn check(ctx: &Context, tm: &Tm, expected_ty: &core::Vty) -> Result<core::Tm, ElabError> {
    match (&tm.data, expected_ty) {
        (_, _) => {
            let (tm1, vty1) = infer(ctx, tm)?;
            equate_ty(&tm.location, &vty1, expected_ty)?;
            Ok(tm1)
        }
    }
}

pub fn infer(ctx: &Context, tm: &Tm) -> Result<(core::Tm, core::Vty), ElabError> {
    match &tm.data {
        TmData::Let { name, head, body } => {
            let (head_tm, head_vty) = infer(ctx, head)?;
            // the head needs to be evaluated, so it can be bound in the context
            let head_vtm = eval(&ctx.tms, &head_tm).map_err(ElabError::from_eval_error)?;
            // the body doesn't need to be evaluated
            let (body_tm, body_vty) = infer(&ctx.bind_def(name.clone(), head_vty, head_vtm), body)?;

            Ok((
                core::Tm::new(
                    tm.location.clone(),
                    core::TmData::Let {
                        head: Rc::new(head_tm),
                        body: Rc::new(body_tm),
                    },
                ),
                body_vty,
            ))
        }
        TmData::Name { name } => match ctx.lookup(name.clone()) {
            Some((index, vty)) => Ok((
                core::Tm::new(tm.location.clone(), core::TmData::Var { index }),
                vty,
            )),
            None => Err(ElabError::new(&tm.location, "unbound name")),
        },
        TmData::BoolLit { b } => Ok((
            core::Tm::new(tm.location.clone(), core::TmData::BoolLit { b: *b }),
            core::Vty::BoolTy,
        )),
        TmData::IntLit { i } => Ok((
            core::Tm::new(tm.location.clone(), core::TmData::IntLit { i: *i }),
            core::Vty::IntTy,
        )),
        TmData::StrLit { s } => Ok((
            core::Tm::new(tm.location.clone(), core::TmData::StrLit { s: s.clone() }),
            core::Vty::StrTy,
        )),
        TmData::FunTy { args, body } => Ok((
            core::Tm::new(
                tm.location.clone(),
                core::TmData::FunTy {
                    args: args
                        .iter()
                        .map(|arg| {
                            // each arg must be a Type!
                            Ok(check(ctx, arg, &core::Vty::Univ)?)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    // the body must also be a type!
                    body: Rc::new(check(ctx, body, &core::Vty::Univ)?),
                },
            ),
            core::Vty::Univ,
        )),
        TmData::FunLit { args, ty, body } => {
            // then, make sure each param has a type associated which is actually a type
            let param_tms = args
                .iter()
                .map(|arg| Ok((arg.name.clone(), check(ctx, &arg.ty, &core::Vty::Univ)?)))
                .collect::<Result<Vec<_>, _>>()?;

            let arg_tms = param_tms.iter().map(|(_, a)| a.clone()).collect::<Vec<_>>();
            let arg_vtys = arg_tms
                .clone()
                .iter()
                .map(|arg| eval(&ctx.tms, arg))
                .collect::<Result<Vec<_>, _>>()
                .map_err(ElabError::from_eval_error)?;

            // create a new context with all the parameters bound
            let new_ctx = param_tms
                .clone()
                .iter()
                .try_fold(ctx.clone(), |ctx0, (name, ty)| {
                    // evaluate the type
                    let vty = eval(&ctx.tms, ty).map_err(ElabError::from_eval_error)?;
                    // bind it in the context
                    Ok(ctx0.bind_param(name.clone(), vty))
                })?;

            // make sure the function's type is in fact a type in this new context
            // (so we have access to all the parameters we've just bound! important!)
            let ty = check(&new_ctx, ty, &core::Vty::Univ)?;
            let vty = eval(&new_ctx.tms, &ty).map_err(ElabError::from_eval_error)?;
            // then, make sure the body's type corresponds to the Vty
            let body_tm = check(&new_ctx, body, &vty)?;

            Ok((
                core::Tm::new(
                    tm.location.clone(),
                    core::TmData::FunLit {
                        args: arg_tms,
                        body: Rc::new(body_tm),
                    },
                ),
                core::Vty::FunTy {
                    args: arg_vtys,
                    body: Rc::new(vty),
                },
            ))
        }
        TmData::FunApp { head, args } => match infer(ctx, head)? {
            (head_tm, head_vty) => match head_vty {
                core::Vtm::FunTy {
                    args: args_ty,
                    body: body_vty,
                } => {
                    // elaborate each argument, and make sure they all
                    // correspond to the function's argument type
                    let arg_tms = args
                        .iter()
                        .zip(args_ty)
                        .map(|(arg, arg_ty)| {
                            let arg_tm = check(ctx, arg, &arg_ty)?;

                            Ok(arg_tm)
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok((
                        core::Tm::new(
                            tm.location.clone(),
                            core::TmData::FunApp {
                                head: Rc::new(head_tm),
                                args: arg_tms,
                            },
                        ),
                        body_vty.as_ref().clone(),
                    ))
                }
                _ => Err(ElabError::new(
                    &tm.location,
                    &format!(
                        "trying to apply something as a function when it's a {}",
                        head_vty
                    ),
                )),
            },
        },
    }
}
