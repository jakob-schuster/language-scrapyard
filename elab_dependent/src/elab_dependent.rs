use std::rc::Rc;

use crate::util::Env;
use core::{semantics, Index};

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
        ty1: Rc<Tm>,
        ty2: Rc<Tm>,
    },

    // Function literals: [ fun x => f x ]
    FunLit {
        pats: Vec<Pattern>,
        tm: Rc<Tm>,
    },

    // Function applications: [ f x ]
    FunApp {
        tm: Rc<Tm>,
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
    // Returns the next variable that will be bound in the context after
    // calling bind_def or bind_param
    fn next_var(&self) -> semantics::Neu {
        semantics::Neu::Var(self.size)
    }

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
fn check(ctx: Context, tm: Tm, expected_ty: core::Tm) -> core::Tm {
    match (tm, expected_ty) {
        (
            Tm::Let {
                name,
                def_ty,
                def,
                body,
            },
            expected_ty,
        ) => {
            let def_ty = check(ctx, def_ty.as_ref().clone(), core::Tm::Univ);

            todo!()
        }

        // For anything else, try inferring the type of the term, then checking
        // to see i fth einferred type is the same as the expected type.
        //
        // Instead of using conversion checking, extensions to this type system
        // could trigger unification or try to coerce the term to the expected
        // type here.
        (tm, expected_ty) => {
            let (tm, ty) = infer(ctx, tm);

            todo!()
        }
    }
}

fn infer(ctx: Context, tm: Tm) -> (core::Tm, semantics::Vtm) {
    todo!()
}

mod core {
    use std::rc::Rc;

    pub type Level = usize;
    pub type Index = usize;

    pub type Name = Option<String>;

    pub enum Tm {
        // Let bindings (for sharing definitions inside terms)
        Let {
            name: Name,
            tm1: Rc<Tm>,
            tm2: Rc<Tm>,
        },

        // Terms annotated with types
        Ann {
            tm: Rc<Tm>,
            ty: Rc<Tm>,
        },

        // Variables
        Var(Index),

        // Universe (i.e. the type of types)
        Univ,

        // Dependent function types
        FunType {
            name: Name,
            ty1: Rc<Tm>,
            ty2: Rc<Tm>,
        },

        // Function literals (i.e. lambda expressions)
        FunLit {
            name: Name,
            tm: Rc<Tm>,
        },

        // Function application
        FunApp {
            tm1: Rc<Tm>,
            tm2: Rc<Tm>,
        },
    }

    pub mod semantics {
        use std::rc::Rc;

        use super::Level;
        use super::Name;

        // Terms in weak head normal form
        #[derive(Clone)]
        pub enum Vtm {
            // Neutral terms
            Neu(Neu),

            Univ,
            FunType {
                name: Name,
                ty1: Rc<Vtm>,
                f: Rc<dyn Fn(&Vtm) -> Vtm>,
            },
        }

        #[derive(Clone)]
        pub enum Neu {
            Var(Level),
            FunApp { neu: Rc<Neu>, vtm: Rc<Vtm> },
        }
    }
}
