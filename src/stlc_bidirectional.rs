// Bidirectional elaboration for simply-typed lambda calculus, based on brendan's language garden!

use std::{collections::HashMap, rc::Rc};

pub fn main() {
    println!("ehlo");
    let tm1 = Tm::Let(
        "x".to_string(),
        vec![],
        None,
        Rc::new(Tm::IntLit(4)),
        Rc::new(Tm::IfThenElse {
            guard: Rc::new(Tm::Op2 {
                op: Op2::Eq,
                t1: Rc::new(Tm::Name("x".to_string())),
                t2: Rc::new(Tm::IntLit(3)),
            }),
            t1: Rc::new(Tm::IntLit(1)),
            t2: Rc::new(Tm::IntLit(2)),
        }),
    );

    let tm =
    //     Tm::Let(
    //     "x".to_string(),
    //     vec![],
    //     None,
    //     Rc::new(Tm::BoolLit(false)),
    //     Rc::new(Tm::IfThenElse {
    //         guard: Rc::new(Tm::BoolLit(true)),
    //         t1: Rc::new(Tm::IntLit(0)),
    //         t2: Rc::new(Tm::IntLit(1)),
    //     }),
    // );
    //
    Tm::Let(
        "x".to_string(),
        vec![],
        Some(Ty::Name("Int".to_string())),
        Rc::new(Tm::IntLit(3)),
        Rc::new(Tm::Op2 { op: Op2::Eq, t1: Rc::new(Tm::Name("x".to_string())), t2: Rc::new(Tm::IntLit(0)) }),
    );
    // Tm::Op2 {
    //     op: Op2::Eq,
    //     t1: Rc::new(Tm::IntLit(1)),
    //     t2: Rc::new(Tm::IntLit(3)),
    // };

    let ctm = elab_check(&Context::default(), &tm, &core::Ty::BoolType);

    println!("{}", ctm);

    let vtm = core::eval(core::Env::default(), ctm);

    println!("{}", vtm);
}

// Types in the surface language
#[derive(Clone, Debug, PartialEq)]
enum Ty {
    Name(String),
    FunType { from: Rc<Ty>, to: Rc<Ty> },
}

// Terms in the surface language
enum Tm {
    Name(String),
    Let(String, Vec<Param>, Option<Ty>, Rc<Tm>, Rc<Tm>),
    Ann {
        tm: Rc<Tm>,
        ty: Ty,
    },
    FunLit {
        params: Vec<Param>,
        tm: Rc<Tm>,
    },
    BoolLit(bool),
    IntLit(i32),
    App {
        fun: Rc<Tm>,
        arg: Rc<Tm>,
    },
    IfThenElse {
        guard: Rc<Tm>,
        t1: Rc<Tm>,
        t2: Rc<Tm>,
    },
    Op2 {
        op: Op2,
        t1: Rc<Tm>,
        t2: Rc<Tm>,
    },
    Op1 {
        op: Op1,
        t: Rc<Tm>,
    },
}

// Parameters, with optional type annotations
struct Param {
    name: String,
    ty: Option<Ty>,
}

enum Op2 {
    Eq,
    Add,
    Sub,
    Mul,
}

enum Op1 {
    Neg,
}

// A stack of bindings currently in scope
#[derive(Default)]
struct Context {
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

fn elab_ty(ty: &Ty) -> core::Ty {
    match ty {
        Ty::Name(n) => match &n[..] {
            "Bool" => core::Ty::BoolType,
            "Int" => core::Ty::IntType,

            _ => panic!("unbound type {}", n),
        },
        Ty::FunType { from, to } => core::Ty::FunType {
            from: Rc::new(elab_ty(from)),
            to: Rc::new(elab_ty(to)),
        },
    }
}

fn elab_check(ctx: &Context, tm: &Tm, ty: &core::Ty) -> core::Tm {
    println!("elab_check");
    match tm {
        Tm::Let(def_name, params, def_body_ty, def_body, body) => {
            println!("{def_name}");
            let (def_c, def_cty) = elab_infer_fun_lit(ctx, params, def_body_ty, def_body);
            let body_c = elab_check(&ctx.with(def_name.clone(), def_cty.clone()), body, ty);
            println!("{def_c} , {body_c}");
            core::Tm::Let(def_name.clone(), def_cty, Rc::new(def_c), Rc::new(body_c))
        }
        Tm::FunLit { params, tm } => elab_check_fun_lit(ctx, params, tm, ty),
        Tm::IfThenElse { guard, t1, t2 } => {
            let core_guard = elab_check(ctx, guard, &core::Ty::BoolType);
            let core_t1 = elab_check(ctx, t1, ty);
            let core_t2 = elab_check(ctx, t2, ty);

            core::Tm::BoolElim(Rc::new(core_guard), Rc::new(core_t1), Rc::new(core_t2))
        }

        // Fall back to type inference
        _ => {
            let (tm1, ty1) = elab_infer(ctx, tm);
            assert_eq!(ty.clone(), ty1);

            tm1
        }
    }
}

fn elab_infer(ctx: &Context, tm: &Tm) -> (core::Tm, core::Ty) {
    match tm {
        Tm::Name(name) => match ctx.get(name) {
            Some((index, ty)) => (core::Tm::Var(*index), ty.clone()),
            None => panic!("unbound name {name}"),
        },
        Tm::Let(def_name, params, def_body_ty, def_body, body) => {
            let (def_c, def_cty) = elab_infer_fun_lit(ctx, params, def_body_ty, def_body);
            let (body_c, body_cty) = elab_infer(&ctx.with(def_name.clone(), def_cty.clone()), body);

            println!("{def_cty} , {body_cty}");
            (
                core::Tm::Let(def_name.clone(), def_cty, Rc::new(def_c), Rc::new(body_c)),
                body_cty,
            )
        }
        Tm::Ann { tm, ty } => {
            let core_ty = elab_ty(ty);
            (elab_check(ctx, tm, &core_ty), core_ty)
        }
        Tm::FunLit { params, tm } => elab_infer_fun_lit(ctx, params, &None, tm),
        Tm::BoolLit(b) => (core::Tm::BoolLit(*b), core::Ty::BoolType),
        Tm::IntLit(i) => (core::Tm::IntLit(*i), core::Ty::IntType),
        Tm::App { fun, arg } => {
            let (head, head_ty) = elab_infer(ctx, fun);
            let (param_ty, body_ty) = match head_ty {
                core::Ty::FunType { from, to } => (from, to),
                _ => panic!("mismatched types"),
            };

            let arg = elab_check(ctx, arg, &param_ty);

            (
                core::Tm::FunApp(Rc::new(head), Rc::new(arg)),
                (*body_ty).clone(),
            )
        }
        Tm::IfThenElse { .. } => panic!("ambiguous if expression"),
        Tm::Op2 { op, t1, t2 } => {
            let (ct1, cty1) = elab_infer(ctx, t1);
            let (ct2, cty2) = elab_infer(ctx, t2);
            assert_eq!(cty1, cty2);

            let (prim, cty) = match op {
                Op2::Eq => match cty1 {
                    core::Ty::BoolType => (core::prim::Prim::BoolEq, core::Ty::BoolType),
                    core::Ty::IntType => (core::prim::Prim::IntEq, core::Ty::BoolType),

                    _ => panic!("unsupported type {:?}", cty1),
                },
                Op2::Add => {
                    assert_eq!(cty1, core::Ty::IntType);
                    (core::prim::Prim::IntAdd, core::Ty::IntType)
                }
                Op2::Sub => {
                    assert_eq!(cty1, core::Ty::IntType);
                    (core::prim::Prim::IntSub, core::Ty::IntType)
                }
                Op2::Mul => {
                    assert_eq!(cty1, core::Ty::IntType);
                    (core::prim::Prim::IntMul, core::Ty::IntType)
                }
            };

            (core::Tm::PrimApp(prim, vec![ct1, ct2]), cty)
        }
        Tm::Op1 { op, t } => {
            let (ct, cty1) = elab_infer(ctx, t);

            let (prim, cty) = match op {
                Op1::Neg => {
                    assert_eq!(cty1, core::Ty::IntType);
                    (core::prim::Prim::IntNeg, core::Ty::IntType)
                }
            };

            (core::Tm::PrimApp(prim, vec![ct]), cty)
        }
    }
}

fn elab_check_fun_lit(ctx: &Context, params: &[Param], body: &Tm, ty: &core::Ty) -> core::Tm {
    match (params, ty) {
        ([], ty) => elab_check(ctx, body, ty),
        ([Param { name, ty: None }, rest @ ..], core::Ty::FunType { from, to }) => {
            let body_c = elab_check_fun_lit(
                &ctx.with(name.clone(), from.as_ref().clone()),
                rest,
                body,
                to,
            );

            core::Tm::FunLit(name.clone(), from.as_ref().clone(), Rc::new(body_c))
        }
        (
            [Param {
                name,
                ty: Some(param_ty),
            }, rest @ ..],
            core::Ty::FunType { from, to },
        ) => {
            let param_cty = elab_ty(param_ty);

            // verify that the parameter has been assigned a type which makes sense for this function
            assert_eq!(param_cty, from.as_ref().clone());

            let body_c = elab_check_fun_lit(
                &ctx.with(name.clone(), from.as_ref().clone()),
                rest,
                body,
                to,
            );

            core::Tm::FunLit(name.clone(), param_cty, Rc::new(body_c))
        }

        _ => panic!("unexpected parameter"),
    }
}

fn elab_infer_fun_lit(
    ctx: &Context,
    params: &[Param],
    body_ty: &Option<Ty>,
    body: &Tm,
) -> (core::Tm, core::Ty) {
    println!("elab_infer_fun_lit");

    match (params, body_ty) {
        ([], Some(body_ty)) => {
            let body_cty = elab_ty(body_ty);

            (elab_check(ctx, body, &body_cty), body_cty)
        }
        ([], None) => elab_infer(ctx, body),
        ([Param { name, ty: None }, rest @ ..], _) => panic!("ambiguous parameter type"),
        (
            [Param {
                name,
                ty: Some(param_ty),
            }, rest @ ..],
            body_cty1,
        ) => {
            let param_cty = elab_ty(param_ty);

            let (body_c, body_cty) = elab_infer_fun_lit(
                &ctx.with(name.clone(), param_cty.clone()),
                rest,
                body_cty1,
                body,
            );

            (
                core::Tm::FunLit(name.clone(), param_cty.clone(), Rc::new(body_c)),
                core::Ty::FunType {
                    from: Rc::new(param_cty),
                    to: Rc::new(body_cty),
                },
            )
        }
    }
}

mod core {
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
                Vtm::FunLit(name, ty, rc) => format!("fn {name}: {ty} -> [func]").fmt(f),
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
}
