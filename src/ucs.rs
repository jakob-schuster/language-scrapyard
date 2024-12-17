// Implementation of an evaluator for the Ultimate Conditional Syntax (UCS) presented in [this paper](https://dl.acm.org/doi/10.1145/3689746)

use std::rc::Rc;

enum Constructor {}

enum Term {
    If { splits: Rc<Split<TermBranch>> },
}

enum BinOperator {}

enum Pattern {
    Var { v: String },
    Constructor { c: Constructor, pats: Vec<Pattern> },
}

enum TermBranch {
    Then {
        t0: Term,
        t1: Term,
    },
    And {
        t: Term,
        split: Rc<Split<TermBranch>>,
    },
    Is {
        t: Term,
        split: Rc<Split<PatternBranch>>,
    },
    Op {
        t: Term,
        split: Rc<Split<OperatorBranch>>,
    },
    BinOp {
        t: Term,
        binop: BinOperator,
        split: Rc<Split<TermBranch>>,
    },
}

enum PatternBranch {
    Then {
        p: Pattern,
        t: Term,
    },
    And {
        p: Pattern,
        split: Rc<Split<TermBranch>>,
    },
}

enum OperatorBranch {
    Is {
        split: Rc<Split<PatternBranch>>,
    },
    Op {
        binop: BinOperator,
        split: Rc<Split<TermBranch>>,
    },
}

enum Split<T> {
    Seq {
        t: T,
        next: Rc<Split<T>>,
    },
    Let {
        pat: PatternBranch,
        t: Term,
        split: Rc<Split<T>>,
    },
    Else {
        t: Term,
    },
    Empty,
}
