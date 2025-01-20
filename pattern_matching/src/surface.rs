use std::{collections::HashMap, rc::Rc};

use crate::{
    core,
    util::{self, Located, Location, RecField},
};

pub type Tm = Located<TmData>;
pub enum TmData {
    /// A variable name
    Name {
        n: String,
    },

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
        fields: Vec<RecTmField>,
    },

    /// Record projection. Also known as field access
    RecProj {
        tm: Rc<Tm>,
        name: String,
    },

    /// A let binding, just binds one name to one value in the body
    Let {
        n: String,
        head: Rc<Tm>,
        body: Rc<Tm>,
    },

    /// A match expression. Evaluates to the first branch whose pattern matches
    Match {
        tm: Rc<Tm>,
        branches: Vec<Branch>,
    },
}

pub type RecTmField = RecField<Tm>;

pub struct Branch {
    pub pattern: Pattern,
    pub tm: Tm,
}

pub type Pattern = Located<PatternData>;
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
        fields: Vec<RecPatternField>,
    },

    /// RecLit that can contain any number of other fields
    RecLitWith {
        fields: Vec<RecPatternField>,
    },

    /// Custom matchers?
    StrConstructor {
        regs: Vec<StrConsRegion>,
    },
    FuzzyStringMatch {
        s: String,
    },
}

pub type RecPatternField = RecField<Pattern>;

pub enum StrConsRegion {
    Str { s: String },
    Named { n: String },
}

// A stack of bindings currently in scope
#[derive(Default, Clone)]
pub struct Context {
    map: HashMap<String, (usize, core::Ty)>,
}
impl Context {
    fn with(&self, name: String, t: core::Ty) -> Context {
        let mut new_map = self.map.clone();
        let index = self.map.len();

        new_map.insert(name, (index, t));

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
    if ty1.equiv(ty2) {
        Ok(())
    } else {
        Err(ElabError::new(
            location,
            &format!("mismatched types: expected {}, found {}", ty2, ty1),
        ))
    }
}

pub fn elab_check(ctx: &Context, tm: &Tm, ty: &core::Ty) -> Result<core::Tm, ElabError> {
    match &tm.data {
        // fall back to type inference
        _ => {
            let (tm1, ty1) = elab_infer(ctx, tm)?;
            equate_ty(&tm.location, &ty.clone(), &ty1)?;

            Ok(tm1)
        }
    }
}

pub fn elab_infer(ctx: &Context, tm: &Tm) -> Result<(core::Tm, core::Ty), ElabError> {
    match &tm.data {
        TmData::Name { n } => match ctx.get(n) {
            Some((index, ty)) => Ok((core::Tm::Var { index: *index }, ty.clone())),
            None => Err(ElabError::new(&tm.location, &format!("unbound name: {n}"))),
        },
        TmData::BoolLit { b } => Ok((core::Tm::BoolLit { b: *b }, core::Ty::Bool)),
        TmData::IntLit { i } => Ok((core::Tm::IntLit { i: *i }, core::Ty::Int)),
        TmData::StrLit { s } => Ok((core::Tm::StrLit { s: s.clone() }, core::Ty::Str)),
        TmData::RecLit { fields } => {
            let mut ty_fields = vec![];
            let mut tm_fields = vec![];

            // get the ty and tm of each field
            for field in fields {
                let (tm, ty) = elab_infer(ctx, &field.data)?;

                ty_fields.push(core::RecTyField {
                    name: field.name.clone(),
                    data: ty,
                });

                tm_fields.push(core::RecTmField {
                    name: field.name.clone(),
                    data: tm,
                });
            }

            Ok((
                core::Tm::RecLit { fields: tm_fields },
                core::Ty::Rec { fields: ty_fields },
            ))
        }

        TmData::Let { n, head, body } => {
            let (head_core, head_core_ty) = elab_infer(ctx, head)?;
            let (body_core, body_core_ty) = elab_infer(&ctx.with(n.clone(), head_core_ty), body)?;

            Ok((
                core::Tm::Let {
                    n: n.clone(),
                    head: Rc::new(head_core),
                    body: Rc::new(body_core),
                },
                body_core_ty,
            ))
        }
        TmData::Match { tm, branches } => {
            // need to also check that each branch's pattern's ty matches the ty of tm
            let (core_tm, core_ty) = elab_infer(ctx, tm)?;

            let mut final_ty = core::Ty::Any;
            let mut final_branches = vec![];
            for branch in branches {
                let (core_branch, core_branch_ty) = elab_infer_branch(ctx, branch, &core_ty)?;

                // check that we can equate the branches
                equate_ty(&branch.tm.location, &core_branch_ty, &final_ty)?;
                // then, hold on to whichever is more precise
                final_ty = final_ty.most_precise(&core_branch_ty).clone();
                // and keep all the core branches
                final_branches.push(core_branch);
            }

            // then, we would check for full coverage / completeness ... but let's not for now

            Ok((
                core::Tm::Match {
                    tm: Rc::new(core_tm),
                    branches: final_branches,
                },
                final_ty,
            ))
        }

        TmData::RecProj { tm, name } => {
            let (core_tm, core_ty) = elab_infer(ctx, tm)?;

            match core_ty {
                core::Ty::Rec { fields } => match core::proj_ty(&fields, name) {
                    Some(ty) => Ok((
                        core::Tm::RecProj {
                            tm: Rc::new(core_tm),
                            name: name.clone(),
                        },
                        ty,
                    )),
                    None => Err(ElabError::new(
                        &tm.location,
                        "field name not present in record!",
                    )),
                },
                _ => Err(ElabError::new(&tm.location, "not a record!")),
            }
        }
    }
}

fn elab_infer_branch(
    ctx: &Context,
    branch: &Branch,
    ty: &core::Ty,
) -> Result<(core::Branch, core::Ty), ElabError> {
    // need to check that this branch's pattern's ty matches the ty of tm
    let (matcher, ty1, bindings) = elab_infer_pattern(ctx, &branch.pattern)?;
    equate_ty(&branch.pattern.location, &ty1, ty)?;

    // extend the context to include the new bindings.
    let new_ctx = bindings.iter().fold(ctx.clone(), |ctx0, (name, t)| {
        ctx0.with(name.clone(), t.clone())
    });

    // then need to check the type of this branch's body, and return that
    let (body, body_ty) = elab_infer(&new_ctx, &branch.tm)?;
    Ok((core::Branch { matcher, body }, body_ty))
}

fn elab_infer_pattern(
    ctx: &Context,
    pattern: &Pattern,
) -> Result<
    (
        Rc<dyn core::matcher::Matcher>,
        core::Ty,
        Vec<(String, core::Ty)>,
    ),
    ElabError,
> {
    match &pattern.data {
        PatternData::Named { name, pattern } => {
            // first, try to match the inner pattern
            let (pattern_matcher, ty, mut binds) = elab_infer_pattern(ctx, pattern)?;
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
        PatternData::Hole => Ok((Rc::new(core::matcher::Succeed {}), core::Ty::Any, vec![])),
        PatternData::BoolLit { b } => {
            let vtm = core::Vtm::Bool { b: *b };
            Ok((
                Rc::new(core::matcher::Equal { vtm }),
                core::Ty::Bool,
                vec![],
            ))
        }
        PatternData::IntLit { i } => {
            let vtm = core::Vtm::Int { i: *i };
            Ok((Rc::new(core::matcher::Equal { vtm }), core::Ty::Int, vec![]))
        }
        PatternData::StrLit { s } => {
            let vtm = core::Vtm::Str { s: s.clone() };

            Ok((Rc::new(core::matcher::Equal { vtm }), core::Ty::Str, vec![]))
        }
        PatternData::FuzzyStringMatch { s } => {
            Ok((Rc::new(core::matcher::Fuzzy {}), core::Ty::Str, vec![]))
        }
        PatternData::StrConstructor { regs } => {
            // get the named regions, in order
            let mut named = vec![];
            for reg in regs {
                match reg {
                    StrConsRegion::Str { s } => {}
                    StrConsRegion::Named { n } => named.push((n.clone(), core::Ty::Str)),
                }
            }

            Ok((
                Rc::new(core::matcher::StrConstructor::new(regs)),
                core::Ty::Str,
                named,
            ))
        }
        PatternData::RecLit { fields } => {
            let (matcher, tys, names): (
                Rc<dyn core::matcher::Matcher>,
                Vec<RecField<core::Ty>>,
                Vec<(String, core::Ty)>,
            ) = fields.iter().try_fold(
                (
                    Rc::new(core::matcher::Succeed {}) as Rc<dyn core::matcher::Matcher>,
                    vec![],
                    vec![],
                ),
                |(acc, tys, names), field| {
                    let (m1, ty1, names1) = elab_infer_pattern(ctx, &field.data)?;

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

            Ok((matcher, core::Ty::Rec { fields: tys }, names))
        }

        // The only difference here to RecLit is in the type elaboration;
        // once type-checked, neither checks that the whole record is covered
        PatternData::RecLitWith { fields } => {
            let (matcher, tys, names): (
                Rc<dyn core::matcher::Matcher>,
                Vec<RecField<core::Ty>>,
                Vec<(String, core::Ty)>,
            ) = fields.iter().try_fold(
                (
                    Rc::new(core::matcher::Succeed {}) as Rc<dyn core::matcher::Matcher>,
                    vec![],
                    vec![],
                ),
                |(acc, tys, names), field| {
                    let (m1, ty1, names1) = elab_infer_pattern(ctx, &field.data)?;

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

            Ok((matcher, core::Ty::RecWith { fields: tys }, names))
        }
    }
}
