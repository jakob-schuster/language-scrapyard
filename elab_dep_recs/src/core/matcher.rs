use crate::util::Env;
use std::rc::Rc;

use super::Vtm;

pub trait Matcher {
    fn evaluate(&self, env: &Env<Vtm>, vtm: &Vtm) -> Option<Vec<Vtm>>;
}

pub struct Chain {
    pub m1: Rc<dyn Matcher>,
    pub m2: Rc<dyn Matcher>,
}
impl Matcher for Chain {
    fn evaluate(&self, env: &Env<Vtm>, vtm: &Vtm) -> Option<Vec<Vtm>> {
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
    fn evaluate(&self, env: &Env<Vtm>, vtm: &Vtm) -> Option<Vec<Vtm>> {
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
    fn evaluate(&self, env: &Env<Vtm>, vtm: &Vtm) -> Option<Vec<Vtm>> {
        Some(vec![])
    }
}

pub struct Bind {}
impl Matcher for Bind {
    fn evaluate(&self, env: &Env<Vtm>, vtm: &Vtm) -> Option<Vec<Vtm>> {
        Some(vec![vtm.clone()])
    }
}

/// Note: this should carry a Vtm
pub struct Equal {
    pub vtm: Vtm,
}
impl Matcher for Equal {
    fn evaluate(&self, env: &Env<Vtm>, vtm: &Vtm) -> Option<Vec<Vtm>> {
        // let vtm0 = super::eval(env, &self.tm);

        if vtm.eq(&self.vtm) {
            Some(vec![])
        } else {
            None
        }
    }
}
