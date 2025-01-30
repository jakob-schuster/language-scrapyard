use itertools::Itertools;

use crate::{
    core::{self, eval, library, EvalError, Vtm, Vty},
    util::{self, Env, Located, Location, RecField},
};
use std::fmt::Display;
use std::rc::Rc;

pub type Tm = Located<TmData>;
pub type Ty = Tm;
#[derive(Clone)]
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
    // Foreign functions have arguments and a return type,
    // but they only contain an index into a built-in library
    FunForeign {
        args: Vec<Param>,
        ty: Rc<Ty>,
        f: usize,
    },
    FunApp {
        head: Rc<Tm>,
        args: Vec<Tm>,
    },

    RecTy {
        fields: Vec<RecField<Tm>>,
    },
    RecLit {
        fields: Vec<RecField<Tm>>,
    },
    RecProj {
        tm: Rc<Tm>,
        name: String,
    },
    /// A match expression. Evaluates to the first branch whose pattern matches
    Match {
        tm: Rc<Tm>,
        branches: Vec<Branch>,
    },
}

#[derive(Clone)]
pub struct Branch {
    pub pattern: Pattern,
    pub tm: Tm,
}

pub type Pattern = Located<PatternData>;
#[derive(Clone)]
pub enum PatternData {
    /// Named
    Named {
        name: String,
        pattern: Rc<Pattern>,
    },

    /// Holes - match anything
    Hole,

    /// Literals
    BoolLit {
        b: bool,
    },
    IntLit {
        i: i32,
    },
    StrLit {
        s: String,
    },
    RecLit {
        fields: Vec<RecField<Pattern>>,
    },
}

#[derive(Clone)]
pub struct Param {
    pub name: String,
    pub ty: Ty,
}

/// An error that will be raised if there was a problem in the surface syntax,
/// usually as a result of type errors. This is normal, and should be rendered
/// nicely to the programmer.
#[derive(Debug, Clone)]
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
        let index = self.size - 1 - self.names.find_last(&name)?;
        let vty = self.tys.get_index(index);

        Some((index, vty.clone()))
    }

    pub fn standard_library() -> Context {
        let entries = vec![
            ("Type", core::TmData::Univ, core::TmData::Univ),
            ("Bool", core::TmData::Univ, core::TmData::BoolTy),
            ("Int", core::TmData::Univ, core::TmData::IntTy),
            ("Str", core::TmData::Univ, core::TmData::StrTy),
        ];

        let a = entries
            .iter()
            .try_fold(Context::default(), |ctx, (name, ty, tm)| {
                let ty1 = core::eval(&ctx.tms, &core::Tm::new(Location::new(0, 0), ty.clone()))?;
                let tm1 = core::eval(&ctx.tms, &core::Tm::new(Location::new(0, 0), tm.clone()))?;

                Ok::<Context, EvalError>(ctx.bind_def(name.to_string(), ty1, tm1))
            })
            .expect("could not evaluate standard library!");
        a
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.size {
            format!(
                "({}, {}, {}), ",
                self.names.get_level(i),
                self.tms.get_level(i),
                self.tys.get_level(i)
            )
            .fmt(f)?;
        }

        Ok(())
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
                            check(ctx, arg, &core::Vty::Univ)
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

            // then, we need to see if the vty is concrete or if it's neutral
            let (body_tm, vty1) = match vty {
                core::Vtm::Ntm { ntm } => {
                    // if it's neutral, we need to create something special:
                    // a new vty that is just a function from some arguments
                    // to a new vty (which we expect to be concrete)

                    // we also need to just infer the body's type, and
                    // keep that too; once the function is called and given
                    // arguments, these can be compared
                    let (body_tm, body_vty) = infer(&new_ctx, body)?;
                    (
                        body_tm,
                        core::Vtm::FunReturnTyAwaiting {
                            data: core::FunData {
                                env: ctx.tms.clone(),
                                body: ty,
                            },
                            expected_ty: Rc::new(body_vty),
                        },
                    )
                }
                _ => {
                    // then, make sure the body's type corresponds to the Vty
                    let body_tm = check(&new_ctx, body, &vty)?;
                    (body_tm, vty)
                }
            };

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
                    body: Rc::new(vty1),
                },
            ))
        }

        TmData::FunForeign { args, ty, f } => {
            // then, make sure each param has a type associated which is actually a type
            let param_tms = args
                .iter()
                .map(|arg| Ok((arg.name.clone(), check(ctx, &arg.ty, &core::Vty::Univ)?)))
                .collect::<Result<Vec<_>, _>>()?;

            let arg_vtys = param_tms
                .clone()
                .iter()
                .map(|(_, arg)| eval(&ctx.tms, arg))
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

            Ok((
                core::Tm::new(
                    tm.location.clone(),
                    core::TmData::FunForeign {
                        f: library::foreign(&tm.location, *f)
                            .map_err(ElabError::from_eval_error)?,
                    },
                ),
                core::Vty::FunTy {
                    args: arg_vtys,
                    body: Rc::new(vty),
                },
            ))
        }

        TmData::FunApp { head, args } => {
            let (head_tm, head_vty) = infer(ctx, head)?;
            match head_vty {
                core::Vtm::FunTy {
                    args: args_ty,
                    body: body_vty,
                } => {
                    // first make sure the argument lists are the same length
                    if !args.len().eq(&args_ty.len()) {
                        return Err(ElabError::new(
                            &tm.location,
                            &format!(
                                "function was given {} arguments, expected {}",
                                args.len(),
                                args_ty.len()
                            ),
                        ));
                    }

                    // elaborate each argument, and make sure they all
                    // correspond to the function's argument type
                    // - and if any are neutrals, the whole thing is a neutral

                    let arg_tms = args
                        .iter()
                        .zip(args_ty)
                        .map(|(arg, arg_ty)| {
                            let arg_tm = check(ctx, arg, &arg_ty)?;

                            Ok(arg_tm)
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    // check if the return type needs to be evaluated
                    let body_vty1 = match body_vty.as_ref() {
                        Vtm::FunReturnTyAwaiting { data, expected_ty } => {
                            let arg_vtms = arg_tms
                                .iter()
                                .map(|arg| eval(&ctx.tms, arg))
                                .collect::<Result<Vec<_>, _>>()
                                .map_err(ElabError::from_eval_error)?;
                            // apply the vty function to get the concrete vty out
                            let vty = data.app(&arg_vtms).map_err(ElabError::from_eval_error)?;

                            // now, see if the function type actually matched what its body was going to be
                            equate_ty(&tm.location, &vty, expected_ty)?;

                            Ok(vty)
                        }
                        _ => Ok(body_vty.as_ref().clone()),
                    }?;

                    Ok((
                        core::Tm::new(
                            tm.location.clone(),
                            core::TmData::FunApp {
                                head: Rc::new(head_tm),
                                args: arg_tms,
                            },
                        ),
                        body_vty1,
                    ))
                }
                _ => Err(ElabError::new(
                    &tm.location,
                    &format!(
                        "trying to apply something as a function when it's a {}",
                        head_vty
                    ),
                )),
            }
        }
        TmData::RecTy { fields } => {
            // first, check that all the fields are types
            Ok((
                core::Tm::new(
                    tm.location.clone(),
                    core::TmData::RecTy {
                        fields: fields
                            .iter()
                            .map(|field| {
                                Ok(RecField::new(
                                    field.name.clone(),
                                    check(ctx, &field.data, &core::Vtm::Univ)?,
                                ))
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    },
                ),
                core::Vtm::Univ,
            ))
        }
        TmData::RecLit { fields } => {
            let new_fields = fields
                .iter()
                .map(|field| {
                    let (ctm, vty) = infer(ctx, &field.data)?;

                    Ok((
                        RecField::new(field.name.clone(), ctm),
                        RecField::new(field.name.clone(), vty),
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok((
                core::Tm::new(
                    tm.location.clone(),
                    core::TmData::RecLit {
                        fields: new_fields.iter().map(|(f, _)| f.clone()).collect(),
                    },
                ),
                core::Vtm::RecTy {
                    fields: new_fields.iter().map(|(_, f)| f.clone()).collect(),
                },
            ))
        }
        TmData::RecProj { tm, name } => {
            // first, get the value and type of the record
            let (core_tm, core_ty) = infer(ctx, tm)?;

            match core_ty {
                // make sure it is a record
                core::Vtm::RecTy { fields } => {
                    match fields.iter().find(|field| field.name.eq(name)) {
                        // then, make sure the field exists
                        Some(RecField {
                            name: _,
                            data: field_ty,
                        }) => Ok((
                            core::Tm::new(
                                tm.location.clone(),
                                core::TmData::RecProj {
                                    tm: Rc::new(core_tm),
                                    name: name.clone(),
                                },
                            ),
                            field_ty.clone(),
                        )),
                        // otherwise, an error
                        None => Err(ElabError::new(
                            &tm.location,
                            "trying to access non-existent field",
                        )),
                    }
                }
                // otherwise, this is an error
                _ => Err(ElabError::new(
                    &tm.location,
                    "trying to access field of a non-record",
                )),
            }
        }
        TmData::Match { tm, branches } => {
            // need to also check that each branch's pattern's ty matches the ty of tm
            let (core_tm, core_ty) = infer(ctx, tm)?;

            let mut final_ty = core::Vtm::AnyTy;

            let mut final_branches = vec![];
            for branch in branches {
                let (core_branch, core_branch_ty) = infer_branch(ctx, branch, &core_ty)?;

                // check that we can equate the branches
                equate_ty(&branch.tm.location, &core_branch_ty, &final_ty)?;
                // then, hold on to whichever is more precise
                final_ty = final_ty.most_precise(&core_branch_ty).clone();
                // and keep all the core branches
                final_branches.push(core_branch);
            }

            // then, we would check for full coverage / completeness ... but let's not for now

            Ok((
                core::Tm::new(
                    tm.location.clone(),
                    core::TmData::Match {
                        tm: Rc::new(core_tm),
                        branches: final_branches,
                    },
                ),
                final_ty,
            ))
        }
    }
}

fn infer_branch(
    ctx: &Context,
    branch: &Branch,
    ty: &core::Vtm,
) -> Result<(core::Branch, core::Vtm), ElabError> {
    // need to check that this branch's pattern's ty matches the ty of tm
    let (matcher, ty1, bindings) = infer_pattern(ctx, &branch.pattern)?;
    equate_ty(&branch.pattern.location, &ty1, ty)?;

    // extend the context to include the new bindings.
    let new_ctx = bindings.iter().fold(ctx.clone(), |ctx0, (name, t)| {
        ctx0.bind_param(name.clone(), t.clone())
    });

    // then need to check the type of this branch's body, and return that
    let (body, body_ty) = infer(&new_ctx, &branch.tm)?;
    Ok((core::Branch { matcher, body }, body_ty))
}

fn infer_pattern(
    ctx: &Context,
    pattern: &Pattern,
) -> Result<
    (
        Rc<dyn core::matcher::Matcher>,
        core::Vtm,
        Vec<(String, core::Vtm)>,
    ),
    ElabError,
> {
    match &pattern.data {
        PatternData::Named { name, pattern } => {
            // first, try to match the inner pattern
            let (pattern_matcher, ty, mut binds) = infer_pattern(ctx, pattern)?;
            // then, apply the name
            binds.push((name.clone(), ty.clone()));
            // and return a chained matcher
            Ok((
                Rc::new(core::matcher::Chain {
                    m1: pattern_matcher,
                    m2: Rc::new(core::matcher::Bind {}),
                }),
                ty,
                binds,
            ))
        }
        PatternData::Hole => Ok((Rc::new(core::matcher::Succeed {}), core::Vtm::AnyTy, vec![])),
        PatternData::BoolLit { b } => {
            let vtm = core::Vtm::Bool { b: *b };
            Ok((
                Rc::new(core::matcher::Equal { vtm }),
                core::Vtm::BoolTy,
                vec![],
            ))
        }
        PatternData::IntLit { i } => {
            let vtm = core::Vtm::Int { i: *i };
            Ok((
                Rc::new(core::matcher::Equal { vtm }),
                core::Vtm::IntTy,
                vec![],
            ))
        }
        PatternData::StrLit { s } => {
            let vtm = core::Vtm::Str { s: s.clone() };
            Ok((
                Rc::new(core::matcher::Equal { vtm }),
                core::Vtm::StrTy,
                vec![],
            ))
        }
        PatternData::RecLit { fields } => {
            let (matcher, tys, names): (
                Rc<dyn core::matcher::Matcher>,
                Vec<RecField<core::Vtm>>,
                Vec<(String, core::Vtm)>,
            ) = fields.iter().try_fold(
                (
                    Rc::new(core::matcher::Succeed {}) as Rc<dyn core::matcher::Matcher>,
                    vec![],
                    vec![],
                ),
                |(acc, tys, names), field| {
                    let (m1, ty1, names1) = infer_pattern(ctx, &field.data)?;

                    let mut tys1 = tys.clone();
                    tys1.push(RecField::new(field.name.clone(), ty1));

                    let names = names.iter().chain(&names1).cloned().collect::<Vec<_>>();

                    Ok((
                        Rc::new(core::matcher::Chain {
                            m1: Rc::new(core::matcher::FieldAccess {
                                name: field.name.clone(),
                                inner: m1,
                            }),
                            m2: acc,
                        }) as Rc<dyn core::matcher::Matcher>,
                        tys1,
                        names,
                    ))
                },
            )?;

            Ok((matcher, core::Vtm::RecTy { fields: tys }, names))
        }
    }
}
