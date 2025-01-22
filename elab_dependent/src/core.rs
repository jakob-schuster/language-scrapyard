use std::{fmt::Display, rc::Rc};

pub type Level = usize;
pub type Index = usize;
fn level_to_index(size: usize, level: Level) -> Index {
    size - level - 1
}

pub type Name = Option<String>;

#[derive(Clone)]
pub enum Tm {
    // Let bindings (for sharing definitions inside terms)
    Let {
        name: Name,
        def: Rc<Tm>,
        body: Rc<Tm>,
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
        param_ty: Rc<Tm>,
        body_ty: Rc<Tm>,
    },

    // Function literals (i.e. lambda expressions)
    FunLit {
        name: Name,
        tm: Rc<Tm>,
    },

    // Function application
    FunApp {
        head: Rc<Tm>,
        arg: Rc<Tm>,
    },
}

impl Display for Tm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub mod semantics {
    use std::rc::Rc;

    use crate::util::Env;

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
            param_ty: Rc<Vtm>,
            body_ty: DeFun,
        },
        FunLit {
            name: Name,
            body: DeFun,
        },
    }

    /// Defunctionalised function type, that captures its environment.
    /// Probably very stupid but will do for now
    #[derive(Clone)]
    pub struct DeFun {
        env: Env<Vtm>,
        body_ty: super::Tm,
    }

    impl DeFun {
        pub fn app(&self, vtm: &Vtm) -> Vtm {
            eval(&self.env.with(vtm.clone()), &self.body_ty)
        }
    }

    #[derive(Clone)]
    pub enum Neu {
        Var(Level),
        FunApp { neu: Rc<Neu>, arg: Rc<Vtm> },
    }

    /// Compute a function application
    fn app(head: &Vtm, arg: &Vtm) -> Vtm {
        match head {
            Vtm::Neu(neu) => Vtm::Neu(Neu::FunApp {
                neu: Rc::new(neu.clone()),
                arg: Rc::new(arg.clone()),
            }),
            Vtm::FunLit { name, body } => body.app(arg),
            _ => panic!("invalid application"),
        }
    }

    /// Evaluation
    ///
    /// Evaluate a term from the syntax into its semantic interpretation
    pub fn eval(env: &Env<Vtm>, tm: &super::Tm) -> Vtm {
        match tm {
            super::Tm::Let { name, def, body } => eval(&env.with(eval(env, def)), body),
            super::Tm::Ann { tm, ty } => eval(env, tm),
            super::Tm::Var(index) => env.get(*index).clone(),
            super::Tm::Univ => Vtm::Univ,
            super::Tm::FunType {
                name,
                param_ty,
                body_ty,
            } => {
                let param_ty1 = eval(env, param_ty);

                Vtm::FunType {
                    name: name.clone(),
                    param_ty: Rc::new(param_ty1),
                    body_ty: DeFun {
                        env: env.clone(),
                        body_ty: body_ty.as_ref().clone(),
                    },
                }
            }
            super::Tm::FunLit { name, tm } => Vtm::FunLit {
                name: name.clone(),
                body: DeFun {
                    env: env.clone(),
                    body_ty: tm.as_ref().clone(),
                },
            },
            super::Tm::FunApp { head, arg } => app(&eval(env, head), &eval(env, arg)),
        }
    }

    // Quotation
    //
    /// Quotation allows us to convert terms from the semantic domain back into
    /// syntax. This can be useful to find the normal form of a term, or when
    /// including terms from the semantics in the syntax during elaboration.
    ///
    /// The size parameter is the number of bindings present in the environment
    /// where the resulting terms should be bound, allowing us to convert
    /// variables in the semantic domain back to an index representation with
    /// level_to_size. It's important to only use the resulting terms at
    /// binding depth that they were quoted at.
    pub fn quote(size: usize, vtm: &Vtm) -> super::Tm {
        match vtm {
            Vtm::Neu(neu) => quote_neu(size, neu),
            Vtm::Univ => super::Tm::Univ,
            Vtm::FunType {
                name,
                param_ty,
                body_ty,
            } => {
                let x = Vtm::Neu(Neu::Var(size));

                super::Tm::FunType {
                    name: name.clone(),
                    param_ty: Rc::new(quote(size, param_ty)),
                    body_ty: Rc::new(quote(size + 1, &body_ty.app(&x))),
                }
            }
            Vtm::FunLit { name, body } => {
                let x = Vtm::Neu(Neu::Var(size));
                super::Tm::FunLit {
                    name: name.clone(),
                    tm: Rc::new(quote(size + 1, &body.app(&x))),
                }
            }
        }
    }

    fn quote_neu(size: usize, neu: &Neu) -> super::Tm {
        match neu {
            Neu::Var(level) => super::Tm::Var(super::level_to_index(size, *level)),
            Neu::FunApp { neu, arg } => super::Tm::FunApp {
                head: Rc::new(quote_neu(size, neu)),
                arg: Rc::new(quote(size, arg)),
            },
        }
    }

    /// Normalisation
    ///
    /// By evaluating a term then quoting the result, we can produce a term
    /// that is reduced as much as possible in the current environment.
    fn normalise(size: usize, env: &Env<Vtm>, tm: &super::Tm) -> super::Tm {
        quote(size, &eval(env, tm))
    }

    /// Conversion Checking
    ///
    /// Checks that two values compute to the same term under the assumption
    /// that both both values have the same type.
    pub fn is_convertible(size: usize, vtm1: &Vtm, vtm2: &Vtm) -> bool {
        match (vtm1, vtm2) {
            (Vtm::Neu(neu1), Vtm::Neu(neu2)) => is_convertible_neu(size, neu1, neu2),
            (Vtm::Univ, Vtm::Univ) => true,
            (
                Vtm::FunType {
                    name: _,
                    param_ty: param_ty1,
                    body_ty: body_ty1,
                },
                Vtm::FunType {
                    name: _,
                    param_ty: param_ty2,
                    body_ty: body_ty2,
                },
            ) => {
                let x = Vtm::Neu(Neu::Var(size));

                is_convertible(size, param_ty1, param_ty2)
                    && is_convertible(size + 1, &body_ty1.app(&x), &body_ty2.app(&x))
            }

            (
                Vtm::FunLit {
                    name: _,
                    body: body1,
                },
                Vtm::FunLit {
                    name: _,
                    body: body2,
                },
            ) => {
                let x = Vtm::Neu(Neu::Var(size));

                is_convertible(size + 1, &body1.app(&x), &body2.app(&x))
            }

            (Vtm::FunLit { name: _, body }, fun_tm) | (fun_tm, Vtm::FunLit { name: _, body }) => {
                let x = Vtm::Neu(Neu::Var(size));

                is_convertible(size, &body.app(&x), &app(fun_tm, &x))
            }

            _ => false,
        }
    }

    fn is_convertible_neu(size: usize, neu1: &Neu, neu2: &Neu) -> bool {
        match (neu1, neu2) {
            (Neu::Var(level1), Neu::Var(level2)) => level1.eq(level2),
            (
                Neu::FunApp {
                    neu: neu1,
                    arg: vtm1,
                },
                Neu::FunApp {
                    neu: neu2,
                    arg: vtm2,
                },
            ) => is_convertible_neu(size, neu1, neu2) && is_convertible(size, vtm1, vtm2),
            _ => false,
        }
    }
}
