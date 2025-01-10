use std::rc::Rc;

use crate::core;
use crate::core::semantics::eval;
use crate::core::{semantics, Index};
use crate::util::Env;

type Pattern = Option<String>;

#[derive(Clone)]
enum Tm {
    // Let expressions: [ let x : A := t; f x ]
    Let {
        name: Pattern,
        def_ty: Rc<Tm>,
        def: Rc<Tm>,
        body: Rc<Tm>,
    },

    // References to named things: [ x ]
    Named(String),

    // Terms annotated with types: [ x : A ]
    Ann {
        tm: Rc<Tm>,
        ty: Rc<Tm>,
    },

    // Universe of types: [ Type ]
    Univ,

    // Function types: [ fun (x : A) -> B x ]
    FunType {
        args: Vec<Arg>,
        body: Rc<Tm>,
    },

    // Function arrow types: [ A -> B ]
    FunArrow {
        param_ty: Rc<Tm>,
        body_ty: Rc<Tm>,
    },

    // Function literals: [ fun x => f x ]
    FunLit {
        names: Vec<Pattern>,
        body: Rc<Tm>,
    },

    // Function applications: [ f x ]
    FunApp {
        head: Rc<Tm>,
        args: Vec<Tm>,
    },
}

#[derive(Clone)]
struct Arg {
    pat: Pattern,
    tm: Tm,
}

// Elaboration
// This is where we implement user-facing type checking, while also translating
// the surface language into the simpler, more explicit core language.
//
// While we could translate syntactic sugar in the parser, by leaving
// this to elaboration time we make it easier to report higher quality error
// messages that are more relevant to what the programmer originally wrote.

struct Context {
    size: core::Level,
    names: Env<core::Name>,
    tys: Env<core::semantics::Vtm>,
    tms: Env<core::semantics::Vtm>,
}

impl Context {
    /// Returns the next variable that will be bound in the context after
    /// calling bind_def or bind_param
    fn next_var(&self) -> semantics::Vtm {
        semantics::Vtm::Neu(semantics::Neu::Var(self.size))
    }

    /// Binds a definition in the context
    fn bind_def(&self, name: core::Name, ty: semantics::Vtm, tm: semantics::Vtm) -> Context {
        Context {
            size: self.size + 1,
            names: self.names.with(name),
            tys: self.tys.with(ty),
            tms: self.tms.with(tm),
        }
    }

    /// Binds a parameter in the context
    fn bind_param(&self, name: core::Name, ty: semantics::Vtm) -> Context {
        self.bind_def(name, ty, self.next_var())
    }

    /// Looks up a name in the context
    fn lookup(&self, name: String) -> Option<(Index, semantics::Vtm)> {
        // Find the index of most recent binding in the context identified by
        // name, starting from the most recent binding. This gives us the
        // de Bruijn index of the variable.
        let index = self.names.find_last(&Some(name))?;
        let vtm = self.tys.get(index);

        Some((index, vtm.clone()))
    }
}

// Bidirectional type checking
//
// The algorithm is structured bidirectionally, divided into mutually
// recursive checking and inference modes. By supplying type
// annotations as early as possible using the checking mode, we can improve
// the locality of type errors, and provide enough control to the algorithm
// to keep type inference decidable even in the presence of 'fancy' types,
// for example dependent types, higher rank types, and subtyping.

/// Elaborate a term in the surface language into a term in the core language
/// in the presence of a type annotation.
fn check(ctx: &Context, tm: &Tm, expected_ty: &semantics::Vtm) -> core::Tm {
    match (tm, expected_ty) {
        // Let expressions
        (
            Tm::Let {
                name,
                def_ty,
                def,
                body,
            },
            expected_ty,
        ) => {
            let core_def_ty = check(ctx, def_ty, &semantics::Vtm::Univ);
            let semantics_def_ty = semantics::eval(&ctx.tms, &core_def_ty);
            let core_def = check(ctx, def, &semantics::Vtm::Univ);
            let semantics_def = semantics::eval(&ctx.tms, &core_def);
            let ctx = ctx.bind_def(name.clone(), semantics_def_ty, semantics_def);

            core::Tm::Let {
                name: name.clone(),
                def: Rc::new(core_def),
                body: Rc::new(check(&ctx, body, expected_ty)),
            }
        }

        // Function literals
        (Tm::FunLit { names, body }, expected_ty) => {
            fn go(
                ctx: &Context,
                names: &[Option<String>],
                body_ty: &semantics::Vtm,
                body: &Rc<Tm>,
            ) -> core::Tm {
                match names {
                    [] => check(ctx, body, body_ty),
                    [name, rest @ ..] => match body_ty {
                        semantics::Vtm::FunType {
                            name: _,
                            param_ty,
                            body_ty,
                        } => {
                            let var = ctx.next_var();
                            let ctx =
                                ctx.bind_def(name.clone(), param_ty.as_ref().clone(), var.clone());
                            core::Tm::FunLit {
                                name: name.clone(),
                                tm: Rc::new(go(&ctx, rest, &body_ty.app(&var), body)),
                            }
                        }
                        _ => panic!("too many parameters in function literal!"),
                    },
                }
            }

            go(ctx, names, expected_ty, body)
        }

        // For anything else, try inferring the type of the term, then checking
        // to see i fth einferred type is the same as the expected type.
        //
        // Instead of using conversion checking, extensions to this type system
        // could trigger unification or try to coerce the term to the expected
        // type here.
        (tm, expected_ty) => {
            let (tm, ty) = infer(ctx, tm);

            if semantics::is_convertible(ctx.size, &ty, expected_ty) {
                tm
            } else {
                panic!(
                    "type mismatch. expected: {} found: {}",
                    semantics::quote(ctx.size, expected_ty),
                    semantics::quote(ctx.size, &ty),
                )
            }
        }
    }
}

/// Elaborate a term in the surface language into a term in the core language,
/// inferring its type
fn infer(ctx: &Context, tm: &Tm) -> (core::Tm, semantics::Vtm) {
    match tm {
        // Let expressions
        Tm::Let {
            name,
            def_ty,
            def,
            body,
        } => {
            let def_ty = check(ctx, def_ty, &semantics::Vtm::Univ);
            let semantics_def_ty = eval(&ctx.tms, &def_ty);
            let def = check(ctx, def, &semantics_def_ty);
            let ctx = ctx.bind_def(name.clone(), semantics_def_ty, eval(&ctx.tms, &def));
            let (body, body_ty) = infer(&ctx, body);

            (
                core::Tm::Let {
                    name: name.clone(),
                    def: Rc::new(def),
                    body: Rc::new(body),
                },
                body_ty,
            )
        }

        // Named terms
        Tm::Named(name) => match ctx.lookup(name.clone()) {
            Some((index, vty)) => (core::Tm::Var(index), vty),
            None => panic!("{} not bound", name),
        },

        // Annotated terms
        Tm::Ann { tm, ty } => {
            let ty = check(ctx, ty, &semantics::Vtm::Univ);
            let ty1 = eval(&ctx.tms, &ty);
            let tm = check(ctx, tm, &ty1);
            (
                core::Tm::Ann {
                    tm: Rc::new(tm),
                    ty: Rc::new(ty),
                },
                ty1,
            )
        }

        // Universes
        Tm::Univ =>
        // We use [Type : Type] here for simplicity, which means this type
        // theory is inconsistent. This is okay for a toy type system, but
        // we'd want to look into using universe levels in an actual
        // implementation.
        // TODO: ask brendan about this. or just google it
        {
            (core::Tm::Univ, semantics::Vtm::Univ)
        }

        // Function types
        Tm::FunType { args, body } => {
            fn go(ctx: &Context, params: &[Arg], body_ty: &Tm) -> core::Tm {
                match params {
                    [] => check(ctx, body_ty, &semantics::Vtm::Univ),
                    [Arg { pat, tm }, rest @ ..] => {
                        let param_ty = check(ctx, tm, &semantics::Vtm::Univ);
                        let ctx = ctx.bind_param(pat.clone(), eval(&ctx.tms, &param_ty));

                        core::Tm::FunType {
                            name: pat.clone(),
                            param_ty: Rc::new(param_ty),
                            body_ty: Rc::new(go(&ctx, rest, body_ty)),
                        }
                    }
                }
            }

            (go(ctx, &args[..], body), semantics::Vtm::Univ)
        }
        Tm::FunArrow { param_ty, body_ty } => {
            let param_ty = check(ctx, param_ty, &semantics::Vtm::Univ);
            let ctx = ctx.bind_param(None, eval(&ctx.tms, &param_ty));
            let body_ty = check(&ctx, body_ty, &semantics::Vtm::Univ);
            (
                core::Tm::FunType {
                    name: None,
                    param_ty: Rc::new(param_ty),
                    body_ty: Rc::new(body_ty),
                },
                semantics::Vtm::Univ,
            )
        }
        Tm::FunLit { names, body } => panic!("ambiguous function literal!"),
        Tm::FunApp { head, args } => {
            let (head, head_ty) = infer(ctx, head);

            args.iter()
                .fold((head, head_ty), |(head, head_ty), arg| match head_ty {
                    semantics::Vtm::FunType {
                        name,
                        param_ty,
                        body_ty,
                    } => {
                        let arg1 = check(ctx, arg, &param_ty);

                        (
                            core::Tm::FunApp {
                                head: Rc::new(head),
                                arg: Rc::new(arg1.clone()),
                            },
                            body_ty.app(&eval(&ctx.tms, &arg1)),
                        )
                    }
                    _ => panic!("ambiguous function literal!"),
                })
        }
    }
}
