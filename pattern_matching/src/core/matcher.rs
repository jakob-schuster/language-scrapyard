use super::{Env, Tm, Vtm};
use std::rc::Rc;

pub trait Matcher {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>>;
}

pub struct Chain {
    pub m1: Rc<dyn Matcher>,
    pub m2: Rc<dyn Matcher>,
}
impl Matcher for Chain {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        let r1 = self.m1.evaluate(env, vtm)?;
        let r2 = self.m2.evaluate(env, vtm)?;

        Some(r1.into_iter().chain(r2).collect::<Vec<_>>())
    }
}

pub struct FieldAccess {
    pub name: String,
    pub inner: Rc<dyn Matcher>,
}
impl Matcher for FieldAccess {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        match vtm {
            Vtm::Rec { fields } => match fields.iter().find(|field| field.name.eq(&self.name)) {
                Some(field) => self.inner.evaluate(env, &field.data),
                None => None,
            },
            _ => None,
        }
    }
}

pub struct Succeed {}
impl Matcher for Succeed {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        Some(vec![])
    }
}

pub struct Bind {}
impl Matcher for Bind {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        Some(vec![vtm.clone()])
    }
}

/// Note: this should carry a Vtm
pub struct Equal {
    pub vtm: Vtm,
}
impl Matcher for Equal {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        // let vtm0 = super::eval(env, &self.tm);

        if vtm.eq(&self.vtm) {
            Some(vec![])
        } else {
            None
        }
    }
}

pub struct Fuzzy {}
impl Matcher for Fuzzy {
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        todo!()
    }
}

pub struct StrConstructor {
    regs: Vec<StrConstructorRegion>,
}
enum StrConstructorRegion {
    Str(String),
    Var,
}
impl StrConstructor {
    pub fn new(regs: &[crate::surface::StrConsRegion]) -> StrConstructor {
        StrConstructor {
            regs: regs
                .iter()
                .map(|r| match r {
                    crate::surface::StrConsRegion::Str { s } => {
                        StrConstructorRegion::Str(s.clone())
                    }
                    crate::surface::StrConsRegion::Named { n } => StrConstructorRegion::Var,
                })
                .collect(),
        }
    }
}
impl Matcher for StrConstructor {
    // This is soaking up my time and looks horrible, so I will do the smart thing and leave it for future work.
    fn evaluate(&self, env: &Env, vtm: &Vtm) -> Option<Vec<Vtm>> {
        enum PrevRegion {
            Str { end: usize },
            Var { start: usize },
        }

        match vtm {
            Vtm::Str { s } => {
                fn f(
                    s: &str,
                    i: usize,
                    regs: &[StrConstructorRegion],
                    prev_opt: Option<PrevRegion>,
                ) -> Option<Vec<Vtm>> {
                    match prev_opt {
                        Some(prev) => match regs {
                            // only one reg.. we need to find the last
                            [reg] => match (reg, prev) {
                                (StrConstructorRegion::Str(s0), PrevRegion::Var { start }) => {
                                    // find the last match - and it MUST be at the end of the string
                                    match s.rfind(s0) {
                                        Some(i0) if i0 + s0.len() == s.len() => {
                                            // exact match found at end of string
                                            todo!()
                                        }
                                        _ => None,
                                    }
                                }
                                // assign the rest of the thing
                                (StrConstructorRegion::Var, PrevRegion::Str { end }) => {
                                    Some(vec![Vtm::Str {
                                        s: s[end..].to_string(),
                                    }])
                                }

                                _ => panic!("two of the same type next to each other!?"),
                            },

                            [reg, rest @ ..] => match (reg, prev) {
                                (StrConstructorRegion::Str(s0), PrevRegion::Var { start }) => {
                                    match s.find(s0) {
                                        Some(i0) => {
                                            let v = [Vtm::Str {
                                                s: s[i..i + i0].to_string(),
                                            }];

                                            Some(
                                                v.into_iter()
                                                    .chain(f(
                                                        &s[i0..],
                                                        i + i0,
                                                        rest,
                                                        Some(PrevRegion::Str { end: i + i0 }),
                                                    )?)
                                                    .collect(),
                                            )
                                        }

                                        // no match
                                        None => None,
                                    }
                                }
                                (StrConstructorRegion::Var, PrevRegion::Str { end }) => {
                                    // just record that this was found, and move on
                                    f(s, i, rest, Some(PrevRegion::Var { start: i }))
                                }

                                _ => panic!("two of the same type next to each other!?"),
                            },

                            // nothing left to match .. we need the string to be empty, or we fail
                            [] => {
                                if s.is_empty() {
                                    match prev {
                                        PrevRegion::Str { end } => Some(vec![]),
                                        PrevRegion::Var { start } => {
                                            Some(vec![Vtm::Str { s: s.to_string() }])
                                        }
                                    }
                                } else {
                                    None
                                }
                            }
                        },
                        None => todo!(),
                    }
                }

                todo!()
            }
            _ => todo!(),
        }
    }
}
