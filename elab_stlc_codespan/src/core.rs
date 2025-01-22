use std::{fmt::Display, rc::Rc};

// De Bruijn index, that represents a variable occurrence by
// the number of binders between the occurrence and the binder it refers to
type Index = usize;

// De Bruijn level that represents a variable occurrence by
// the number of binders from the top of the environment to the binder that
// the occurrence refers to. These do not change their meaning as new
// bindings are added to the environment.
type Level = usize;

// Converts a Level to an Index that is bound in an environment of the supplied size.
// Assumes size > level
fn level_to_index(size: usize, level: Level) -> Index {
    size - level - 1
}

#[derive(Clone, PartialEq, Debug)]
pub enum Ty {
    BoolType,
    IntType,
    FunType { from: Rc<Ty>, to: Rc<Ty> },
}

#[derive(Clone)]
pub enum Tm {
    Var(usize),
    Let(String, Ty, Rc<Tm>, Rc<Tm>),
    FunLit(String, Ty, Rc<Tm>),
    FunApp(Rc<Tm>, Rc<Tm>),
    IntLit(i32),
    BoolLit(bool),
    BoolElim(Rc<Tm>, Rc<Tm>, Rc<Tm>),
    PrimApp(prim::Prim, Vec<Tm>),
}

// Terms in weak head normal form (i.e. values)
#[derive(Clone)]
pub enum Vtm {
    Neu(Rc<Ntm>),
    FunLit(String, Ty, Rc<dyn Fn(&Vtm) -> Vtm>),
    IntLit(i32),
    BoolLit(bool),
}

// Neutral values that could not be reduced to a normal form as a result of
// being stuck on something else that would not reduce further.
//
// For simple (non-dependent) type systems these are not actually required,
// however they allow us to quote terms back to syntax, which is useful
// for pretty printing under binders.
#[derive(Clone)]
enum Ntm {
    Var(Level), // A fresh variable (used when evaluating under a binder)
    FunApp(Rc<Ntm>, Vtm),
    BoolElim(Rc<Ntm>, Vtm, Vtm),
    PrimApp(prim::Prim, Vec<Vtm>),
}

pub mod prim {
    use std::fmt::Display;

    #[derive(Clone)]
    pub enum Prim {
        BoolEq,
        IntEq,

        IntAdd,
        IntSub,
        IntMul,
        IntMod,

        IntNeg,
    }

    impl Display for Prim {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Prim::BoolEq => "#bool-eq",

                Prim::IntEq => "#int-eq",
                Prim::IntAdd => "#int-add",
                Prim::IntSub => "#int-sub",
                Prim::IntMul => "#int-mul",
                Prim::IntMod => "#int-mod",
                Prim::IntNeg => "#int-neg",
            }
            .fmt(f)
        }
    }
}

// Eliminators

fn fun_app(head: &Vtm, arg: &Vtm) -> Vtm {
    match head {
        // If the function head is neutral, nothing we can do
        Vtm::Neu(ntm) => Vtm::Neu(Rc::new(Ntm::FunApp(ntm.clone(), arg.clone()))),

        // If it is a function literal, we can apply it
        Vtm::FunLit(_, _, body) => body(arg),

        // Otherwise, type error
        _ => panic!("expected function"),
    }
}

fn bool_elim(head: &Vtm, vtm0: &Vtm, vtm1: &Vtm) -> Vtm {
    match head {
        // If the if-then-else guard is neutral, nothing we can do
        Vtm::Neu(ntm) => Vtm::Neu(Rc::new(Ntm::BoolElim(
            ntm.clone(),
            vtm0.clone(),
            vtm1.clone(),
        ))),

        // If it's true, follow the first branch
        Vtm::BoolLit(true) => vtm0.clone(),

        // Else, follow the second
        Vtm::BoolLit(false) => vtm1.clone(),

        // Otherwise, type error
        _ => panic!("expected boolean"),
    }
}

fn prim_app(prim: &prim::Prim) -> Rc<dyn Fn(&[Vtm]) -> Vtm> {
    match prim {
        prim::Prim::BoolEq => Rc::new(|args: &[Vtm]| match args {
            [Vtm::BoolLit(b1), Vtm::BoolLit(b2)] => Vtm::BoolLit(b1 == b2),
            _ => panic!("bad argument types"),
        }),
        prim::Prim::IntEq => Rc::new(|args: &[Vtm]| match args {
            [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::BoolLit(i1 == i2),
            _ => panic!("bad argument types"),
        }),
        prim::Prim::IntAdd => Rc::new(|args: &[Vtm]| match args {
            [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 + i2),
            _ => panic!("bad argument types"),
        }),
        prim::Prim::IntSub => Rc::new(|args: &[Vtm]| match args {
            [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 - i2),
            _ => panic!("bad argument types"),
        }),
        prim::Prim::IntMul => Rc::new(|args: &[Vtm]| match args {
            [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 * i2),
            _ => panic!("bad argument types"),
        }),
        prim::Prim::IntMod => Rc::new(|args: &[Vtm]| match args {
            [Vtm::IntLit(i1), Vtm::IntLit(i2)] => Vtm::IntLit(i1 % i2),
            _ => panic!("bad argument types"),
        }),
        prim::Prim::IntNeg => Rc::new(|args: &[Vtm]| match args {
            [Vtm::IntLit(i1)] => Vtm::IntLit(-i1),
            _ => panic!("bad argument types"),
        }),
    }
}

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

    fn get(&self, index: &Index) -> &Vtm {
        match self.vec.get(*index) {
            Some(vtm) => vtm,
            None => panic!("could not index environment"),
        }
    }

    fn len(&self) -> usize {
        self.vec.len()
    }
}

// Evaluate a term from the syntax into its semantic interpretation.
// Currently, demands big copying because of the way the funlit move takes place.
// Change so that funlit takes ownership of the auxiliary data it needs?
pub fn eval(env: Env, tm: Tm) -> Vtm {
    match tm {
        Tm::Var(index) => env.get(&index).clone(),
        Tm::Let(_, _, def, body) => {
            let def_val = eval(env.clone(), (*def).clone());

            eval(env.with(def_val), (*body).clone())
        }
        Tm::FunLit(name, param_ty, body) => {
            let new_env = env.clone();

            Vtm::FunLit(
                name.clone(),
                param_ty.clone(),
                Rc::new(move |arg| eval(env.clone().with(arg.clone()), (*body).clone())),
            )
        }
        Tm::FunApp(head, arg) => {
            let head1 = eval(env.clone(), (*head).clone());
            let arg1 = eval(env, (*arg).clone());

            fun_app(&head1, &arg1)
        }
        Tm::IntLit(i) => Vtm::IntLit(i),
        Tm::BoolLit(b) => Vtm::BoolLit(b),
        Tm::BoolElim(head, tm0, tm1) => {
            let head1 = eval(env.clone(), (*head).clone());

            // For now, we are not letting the evaluation be lazy
            let vtm0 = eval(env.clone(), (*tm0).clone());
            let vtm1 = eval(env.clone(), (*tm1).clone());

            bool_elim(&head1, &vtm0, &vtm1)
        }
        Tm::PrimApp(prim, vec) => (prim_app(&prim))(
            vec.iter()
                .map(|tm| eval(env.clone(), tm.clone()))
                .collect::<Vec<_>>()
                .as_slice(),
        ),
    }
}

// Quotation
//
// Convert terms from the semantic domain back into syntax.
fn quote(size: usize, vtm: &Vtm) -> Tm {
    match vtm {
        Vtm::Neu(neu) => quote_neu(size, neu),
        Vtm::FunLit(_, ty, rc) => todo!(),
        Vtm::IntLit(_) => todo!(),
        Vtm::BoolLit(_) => todo!(),
    }
}

fn quote_neu(size: usize, ntm: &Ntm) -> Tm {
    match ntm {
        Ntm::Var(level) => Tm::Var(level_to_index(size, *level)),
        Ntm::FunApp(head_ntm, vtm) => Tm::FunApp(
            Rc::new(quote_neu(size, head_ntm)),
            Rc::new(quote(size, vtm)),
        ),
        Ntm::BoolElim(guard_ntm, vtm0, vtm1) => {
            let tm0 = quote(size, vtm0);
            let tm1 = quote(size, vtm1);

            Tm::BoolElim(
                Rc::new(quote_neu(size, guard_ntm)),
                Rc::new(tm0),
                Rc::new(tm1),
            )
        }
        Ntm::PrimApp(prim, vec) => Tm::PrimApp(
            prim.clone(),
            vec.iter().map(|a| quote(size, a)).collect::<Vec<_>>(),
        ),
    }
}

// Normalisation
//
// By evaluating a term then quoting the result, we can produce a term that
// is reduced as much as possible in the current environment.
fn normalise(env: Env, tm: Tm) -> Tm {
    quote(env.len(), &eval(env, tm))
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::BoolType => "Bool".fmt(f),
            Ty::IntType => "Int".fmt(f),
            Ty::FunType { from, to } => format!("{} -> {}", from, to).fmt(f),
        }
    }
}

impl Display for Tm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tm::Var(name) => name.fmt(f),
            Tm::Let(name, def_ty, def, body) => {
                format!("let {name}: {def_ty} = {def} in {body}").fmt(f)
            }
            Tm::FunLit(a, ty, rc) => format!("fn {a}: {ty} => {rc}").fmt(f),
            Tm::FunApp(rc, rc1) => format!("({rc})({rc1})").fmt(f),
            Tm::IntLit(i) => format!("{i}").fmt(f),
            Tm::BoolLit(b) => format!("{b}").fmt(f),
            Tm::BoolElim(rc, rc1, rc2) => format!("if {rc} then {rc1} else {rc2}").fmt(f),
            Tm::PrimApp(prim, vec) => format!(
                "{}({})",
                prim,
                vec.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
            .fmt(f),
        }
    }
}

impl Display for Vtm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Vtm::Neu(rc) => rc.fmt(f),
            Vtm::FunLit(name, ty, rc) => format!("fn {name} : {ty}").fmt(f),
            Vtm::IntLit(i) => i.fmt(f),
            Vtm::BoolLit(b) => b.fmt(f),
        }
    }
}

impl Display for Ntm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ntm::Var(name) => name.fmt(f),
            Ntm::FunApp(rc, vtm) => "({rc})({vtm})".fmt(f),
            Ntm::BoolElim(rc, vtm, vtm1) => "if {rc} then {vtm} else {vtm1}".fmt(f),
            Ntm::PrimApp(prim, vec) => format!(
                "({prim})({})",
                vec.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
            .fmt(f),
        }
    }
}
