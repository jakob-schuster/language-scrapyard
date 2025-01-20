use std::{fmt::Display, rc::Rc};

use itertools::Itertools;

use crate::util::RecField;

pub mod matcher;

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
    /// A record type which contains a precise set of fields
    Rec {
        fields: Vec<RecTyField>,
    },
    // A record type which requires certain fields;
    // other fields can also be harmlessly present
    RecWith {
        fields: Vec<RecTyField>,
    },

    Bool,
    Int,
    Str,

    /// Any type; equivalent to all, usable anywhere
    Any,
}

pub type RecTyField = RecField<Ty>;

impl Ty {
    pub fn equiv(&self, other: &Self) -> bool {
        // takes a field name, and looks it up in the list of fields,
        // returning the type if one is found
        let get = |fields: &[RecTyField], name: &str| -> Option<Ty> {
            let mut out = None;
            for field in fields {
                if field.name.eq(name) {
                    out = Some(field.data.clone())
                }
            }
            out
        };

        match (self, other) {
            // if either type is Any, they are equivalent
            (Ty::Any, _) | (_, Ty::Any) => true,

            // if the atomic types are the same, they are equivalent
            (Ty::Bool, Ty::Bool) | (Ty::Int, Ty::Int) | (Ty::Str, Ty::Str) => true,

            // if two record types contain equivalent fields, they are equivalent
            (Ty::Rec { fields: fields1 }, Ty::Rec { fields: fields2 }) => {
                let names1 = fields1.iter().map(|RecField { name, .. }| name);
                let names2 = fields2.iter().map(|RecField { name, .. }| name);

                // names must be exactly the same
                names1.clone().eq(names2)
                // and for each field name, both records should contain it
                // and they should have equivalent types
                    && names1
                        .clone()
                        .all(|name| match (get(fields1, name), get(fields2, name)) {
                            (Some(ty1), Some(ty2)) => ty1.equiv(&ty2),

                            // they don't both contain it
                            _ => false,
                        })
            }

            // if one record type contains equivalent fields to each one
            // required by a record type containing, they are equivalent
            (
                Ty::Rec { fields },
                Ty::RecWith {
                    fields: with_fields,
                },
            )
            | (
                Ty::RecWith {
                    fields: with_fields,
                },
                Ty::Rec { fields },
            ) => {
                let names = fields.iter().map(|RecField { name, .. }| name);
                let with_names = fields.iter().map(|RecField { name, .. }| name);

                // all the names in the RecWith should be present in the Rec
                names.clone().all(|name| with_names.clone().contains(name))
                    && fields
                        .iter()
                        .all(|field| match get(with_fields, &field.name) {
                            // if it contains it, they must be equiv
                            Some(ty) => field.data.equiv(&ty),
                            // if it doesn't contain it, that's fine
                            None => true,
                        })
            }

            // two RecTypeContaining are equivalent if their shared fields are equivalent
            // (any missing field could always be in the margin of flexibility)
            (Ty::RecWith { fields: fields1 }, Ty::RecWith { fields: fields2 }) => {
                let names1 = fields1.iter().map(|RecField { name, .. }| name);
                let names2 = fields2.iter().map(|RecField { name, .. }| name);
                let shared_names = names1.filter(|name| names2.clone().contains(name));

                shared_names
                    .clone()
                    .all(|name| match (get(fields1, name), get(fields2, name)) {
                        // if they both contain it, the types must be equivalent
                        (Some(ty1), Some(ty2)) => ty1.equiv(&ty2),
                        // if just one of them has it, it doesn't matter what the type is
                        (None, Some(_)) | (Some(_), None) => true,
                        // should never happen
                        (None, None) => panic!("neither contains the name?!"),
                    })
            }

            _ => false,
        }
    }

    // assuming two types are equivalent, return a type which is the most precise
    // combination of the two
    pub fn most_precise<'a>(&'a self, other: &'a Self) -> Self {
        // takes a field name, and looks it up in the list of fields,
        // returning the type if one is found
        let get = |fields: &[RecTyField], name: &str| -> Option<Ty> {
            let mut out = None;
            for field in fields {
                if field.name.eq(name) {
                    out = Some(field.data.clone())
                }
            }
            out
        };

        match (self, other) {
            // if either type is Any, the other type is assumed more precise
            (Ty::Any, ty) | (ty, Ty::Any) => ty.clone(),

            // if both are RecWith, collect all the names
            // and take the most precise version of each field
            // { name = String .. } || { name = Any, age = Int .. } => { name = String, age = Int .. }
            (Ty::RecWith { fields: fields1 }, Ty::RecWith { fields: fields2 }) => {
                Ty::RecWith {
                    fields: fields1
                        .iter()
                        .chain(fields2)
                        .map(|RecField { name, .. }| name)
                        .unique()
                        .map(|name| match (get(fields1, name), get(fields2, name)) {
                            // when both Recs contain the field name, take the most precise definition
                            (Some(ty1), Some(ty2)) => {
                                RecField::new(name.clone(), ty1.most_precise(&ty2))
                            }
                            // when just one contains it, take that
                            (None, Some(ty)) | (Some(ty), None) => RecField::new(name.clone(), ty),
                            // this should never happen
                            (None, None) => {
                                panic!("field name error?! where did this name come from?!")
                            }
                        })
                        .collect::<Vec<_>>(),
                }
            }

            // or if only one is RecTypeContaining, choose the other type
            // { name = String, age = Any } || { age = Int, .. } => { name = String, age = Int }
            (
                Ty::RecWith {
                    fields: with_fields,
                },
                Ty::Rec { fields },
            )
            | (
                Ty::Rec { fields },
                Ty::RecWith {
                    fields: with_fields,
                },
            ) => {
                Ty::Rec {
                    // go through the fields from the record (which SHOULD be all the fields)
                    // and take the most precise field from each
                    fields: fields
                        .iter()
                        .map(|field| match get(with_fields, &field.name) {
                            Some(ty) => {
                                RecField::new(field.name.clone(), field.data.most_precise(&ty))
                            }
                            None => field.clone(),
                        })
                        .collect(),
                }
            }

            (Ty::Rec { fields: fields1 }, Ty::Rec { fields: fields2 }) => {
                Ty::Rec {
                    fields: fields1
                        .iter()
                        .chain(fields2)
                        .map(|RecField { name, .. }| name)
                        .unique()
                        .map(|name| match (get(fields1, name), get(fields2, name)) {
                            // when both Recs contain the field name, take the most precise definition
                            (Some(ty1), Some(ty2)) => {
                                RecField::new(name.clone(), ty1.most_precise(&ty2))
                            }
                            // this should never happen
                            _ => panic!(
                                "a record was missing a field?! both should have all fields?!"
                            ),
                        })
                        .collect::<Vec<_>>(),
                }
            }

            // all atomic combinations are equally precise (assuming they are equiv!)
            _ => self.clone(),
        }
    }
}

pub fn proj_ty(fields: &[RecTyField], name: &str) -> Option<Ty> {
    match fields.iter().find(|field| field.name.eq(name)) {
        Some(field) => Some(field.data.clone()),
        None => None,
    }
}

pub enum Tm {
    /// Variables just store their index in the environment
    Var {
        index: usize,
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

    /// A let binding, just binds one name to one value in the body
    Let {
        n: String,
        head: Rc<Tm>,
        body: Rc<Tm>,
    },

    /// A record projection, also called field access
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

pub type RecTmField = RecField<Tm>;

pub struct Branch {
    /// A function from an environment to a list of new bindings, or a failure to match
    pub matcher: Rc<dyn matcher::Matcher>,

    /// A body to evaluate if the match function succeeds
    pub body: Tm,
}

/// Values
#[derive(Clone, PartialEq, Eq)]
pub enum Vtm {
    Bool { b: bool },
    Int { i: i32 },
    Str { s: String },
    Rec { fields: Vec<RecVtmField> },
}

pub type RecVtmField = RecField<Vtm>;

#[derive(Clone, Default)]
pub struct Env {
    vec: Vec<Vtm>,
}

impl Env {
    fn with(&self, vtm: Vtm) -> Env {
        let mut new_vec = self.vec.clone();
        new_vec.push(vtm);

        Env { vec: new_vec }
    }

    fn get(&self, index: &usize) -> &Vtm {
        match self.vec.get(*index) {
            Some(vtm) => vtm,
            None => panic!("could not index environment?!"),
        }
    }

    fn len(&self) -> usize {
        self.vec.len()
    }
}
pub fn eval(env: &Env, tm: &Tm) -> Vtm {
    match tm {
        Tm::Var { index } => env.get(index).clone(),

        Tm::BoolLit { b } => Vtm::Bool { b: *b },
        Tm::IntLit { i } => Vtm::Int { i: *i },
        Tm::StrLit { s } => Vtm::Str { s: s.clone() },
        Tm::RecLit { fields } => {
            // evaluate each field
            let fields = fields
                .iter()
                .map(|field| RecVtmField {
                    name: field.name.clone(),
                    data: eval(env, &field.data),
                })
                .collect();

            Vtm::Rec { fields }
        }

        Tm::Let { n, head, body } => {
            let head_val = eval(env, head);

            eval(&env.with(head_val), body)
        }
        Tm::Match { tm, branches } => {
            let vtm = eval(env, tm);

            // go through each branch
            for branch in branches {
                if let Some(vtms) = branch.matcher.evaluate(env, &vtm) {
                    // bind all the values in the env
                    let new_env = vtms
                        .iter()
                        .fold(env.clone(), |env0, vtm| env0.with(vtm.clone()));

                    // evaluate the arm
                    return eval(&new_env, &branch.body);
                }
            }

            panic!("incomplete match?!")
        }

        Tm::RecProj { tm, name } => {
            // evaluate the tm
            match eval(env, tm) {
                Vtm::Rec { fields } => {
                    for field in fields {
                        if field.name.eq(name) {
                            return field.data;
                        }
                    }

                    panic!("rec did not contain the field {name}?!")
                }
                _ => panic!("tm was the wrong type?!"),
            }
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Bool => "Bool".fmt(f),
            Ty::Int => "Int".fmt(f),
            Ty::Str => "Str".fmt(f),
            Ty::Any => "Any".fmt(f),
            Ty::Rec { fields } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(RecField::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
            Ty::RecWith { fields } => format!(
                "{{ {} .. }}",
                fields
                    .iter()
                    .map(RecField::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
        }
    }
}

impl Display for Vtm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Vtm::Bool { b } => b.fmt(f),
            Vtm::Int { i } => i.fmt(f),
            Vtm::Str { s } => s.fmt(f),
            Vtm::Rec { fields } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(RecField::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .fmt(f),
        }
    }
}
