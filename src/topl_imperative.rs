// A simple imperative language, as described in chapter 2 of Theories of Programming Languages.

use std::{collections::HashMap, rc::Rc};

enum IntExp {
    Val(i32),

    Var(String),

    Neg { e: Rc<IntExp> },

    Add { e1: Rc<IntExp>, e2: Rc<IntExp> },
    Sub { e1: Rc<IntExp>, e2: Rc<IntExp> },
    Mul { e1: Rc<IntExp>, e2: Rc<IntExp> },
    Div { e1: Rc<IntExp>, e2: Rc<IntExp> },
    Mod { e1: Rc<IntExp>, e2: Rc<IntExp> },
}

enum BoolExp {
    Val(bool),

    Eq { e1: Rc<IntExp>, e2: Rc<IntExp> },
    NotEq { e1: Rc<IntExp>, e2: Rc<IntExp> },
    Less { e1: Rc<IntExp>, e2: Rc<IntExp> },
    LessEq { e1: Rc<IntExp>, e2: Rc<IntExp> },
    Greater { e1: Rc<IntExp>, e2: Rc<IntExp> },
    GreaterEq { e1: Rc<IntExp>, e2: Rc<IntExp> },

    Not { e: Rc<BoolExp> },
    And { e1: Rc<BoolExp>, e2: Rc<BoolExp> },
    Or { e1: Rc<BoolExp>, e2: Rc<BoolExp> },
    If { e1: Rc<BoolExp>, e2: Rc<BoolExp> },
    Iff { e1: Rc<BoolExp>, e2: Rc<BoolExp> },
}

enum Comm {
    Assign {
        v: String,
        e: IntExp,
    },
    Skip,
    Seq {
        c1: Rc<Comm>,
        c2: Rc<Comm>,
    },
    If {
        guard: BoolExp,
        c1: Rc<Comm>,
        c2: Rc<Comm>,
    },
    While {
        guard: BoolExp,
        c: Rc<Comm>,
    },
    Print {
        e: IntExp,
    },
}

#[derive(Debug, PartialEq)]
enum Error {
    UnknownVar(String),
}

type Env = HashMap<String, i32>;

impl IntExp {
    fn eval(&self, env: &Env) -> Result<i32, Error> {
        match self {
            IntExp::Val(n) => Ok(*n),

            IntExp::Var(v) => match env.get(v) {
                Some(n) => Ok(*n),
                None => Err(Error::UnknownVar(v.clone())),
            },

            IntExp::Neg { e } => {
                let v = e.eval(env)?;

                Ok(-v)
            }

            IntExp::Add { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 + v2)
            }
            IntExp::Sub { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 - v2)
            }
            IntExp::Mul { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 * v2)
            }
            IntExp::Div { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 / v2)
            }
            IntExp::Mod { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 % v2)
            }
        }
    }
}

impl BoolExp {
    fn eval(&self, env: &Env) -> Result<bool, Error> {
        match self {
            BoolExp::Val(b) => Ok(*b),
            BoolExp::Eq { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 == v2)
            }
            BoolExp::NotEq { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 != v2)
            }
            BoolExp::Less { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 < v2)
            }
            BoolExp::LessEq { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 <= v2)
            }
            BoolExp::Greater { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 > v2)
            }
            BoolExp::GreaterEq { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 >= v2)
            }
            BoolExp::Not { e } => {
                let v = e.eval(env)?;

                Ok(!v)
            }
            BoolExp::And { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 && v2)
            }
            BoolExp::Or { e1, e2 } => {
                let (v1, v2) = (e1.eval(env)?, e2.eval(env)?);

                Ok(v1 || v2)
            }
            BoolExp::If { e1, e2 } => Ok(match (e1.eval(env)?, e2.eval(env)?) {
                (true, v2) => v2,
                (false, _) => true,
            }),
            BoolExp::Iff { e1, e2 } => Ok(matches!(
                (e1.eval(env)?, e2.eval(env)?),
                (true, true) | (false, false)
            )),
        }
    }
}

impl Comm {
    fn exec(&self, env: &Env) -> Result<Env, Error> {
        match self {
            Comm::Assign { v, e } => {
                let mut new_env = env.clone();
                new_env.insert(v.clone(), e.eval(env)?);

                Ok(new_env)
            }
            Comm::Skip => Ok(env.clone()),
            Comm::Seq { c1, c2 } => {
                let new_env = c1.exec(env)?;
                c2.exec(&new_env)
            }
            Comm::If { guard, c1, c2 } => match guard.eval(env)? {
                true => c1.exec(env),
                false => c2.exec(env),
            },
            Comm::While { guard, c } => {
                let mut new_env = env.clone();

                while guard.eval(&new_env)? {
                    new_env = c.exec(env)?;
                }

                Ok(new_env)
            }
            Comm::Print { e } => {
                let v = e.eval(env)?;
                println!("{}", v);

                Ok(env.clone())
            }
        }
    }
}

#[cfg(test)]
mod test {
    // Tests that a simple integer expression evaluates correctly
    // "1 + 3" evaluates to "4"
    #[test]
    fn test_int_add() {
        use super::*;

        let e = IntExp::Add {
            e1: Rc::new(IntExp::Val(1)),
            e2: Rc::new(IntExp::Val(3)),
        };

        let env = HashMap::new();

        assert_eq!(e.eval(&env), Ok(4));
    }

    // Tests that a simple integer expression evaluates correctly
    // "10 * 10" evaluates to "100"
    #[test]
    fn test_int_mul() {
        use super::*;

        let e = IntExp::Mul {
            e1: Rc::new(IntExp::Val(10)),
            e2: Rc::new(IntExp::Val(10)),
        };

        let env = HashMap::new();

        assert_eq!(e.eval(&env), Ok(100));
    }

    // Tests that "not False" evaluates to "True"
    #[test]
    fn test_bool_not() {
        use super::*;

        let e = BoolExp::Not {
            e: Rc::new(BoolExp::Val(false)),
        };

        let env = HashMap::new();

        assert_eq!(e.eval(&env), Ok(true));
    }

    // Tests that evaluating a series of commands produces the expected final state
    // "v := 10; if v > 9 then x := 0 else x := 1" evaluates to the state where "x == 0"
    #[test]
    fn test_comm_sequence() {
        use super::*;

        let comm = Comm::Seq {
            c1: Rc::new(Comm::Assign {
                v: String::from("v"),
                e: IntExp::Val(10),
            }),
            c2: Rc::new(Comm::If {
                guard: BoolExp::Greater {
                    e1: Rc::new(IntExp::Var(String::from("v"))),
                    e2: Rc::new(IntExp::Val(9)),
                },
                c1: Rc::new(Comm::Assign {
                    v: String::from("x"),
                    e: IntExp::Val(0),
                }),
                c2: Rc::new(Comm::Assign {
                    v: String::from("x"),
                    e: IntExp::Val(1),
                }),
            }),
        };

        let env = HashMap::new();
        let result = comm.exec(&env).unwrap();

        assert_eq!(result.get("x"), Some(&0));
    }
}
