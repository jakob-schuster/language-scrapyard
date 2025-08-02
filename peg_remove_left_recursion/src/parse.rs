use crate::surface::{Branch, BranchData, Prog, ProgData, Stmt, StmtData, Tm, TmData};
use crate::util;
use parser::{prog, stmt, tm};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

pub fn parse(string: &str) -> Result<Prog, ParseError> {
    prog(string).map_err(|e| ParseError {
        start: e.location.offset,
        end: e.location.offset + 1,
        message: "Parse error".to_string(),
    })
}
pub fn parse_tm(string: &str) -> Result<Tm, ParseError> {
    tm(string).map_err(|e| ParseError {
        start: e.location.offset,
        end: e.location.offset + 1,
        message: "Parse error".to_string(),
    })
}

peg::parser! {
    grammar parser() for str {
        pub rule prog() -> Prog = located(<prog_data()>)
        pub rule prog_data() -> ProgData
            = _ stmts:(stmt() ** (whitespace_except_newline() ['\n' | ';'] _)) (whitespace_except_newline() ['\n' | ';'] _)? _ { ProgData { stmts: stmts.into_iter().flatten().collect() } }

        pub rule stmt() -> Vec<Stmt> = if_stmt() / stmt:located(<stmt_data()>) { vec![stmt] }
        #[cache]
        pub rule stmt_data() -> StmtData
            = tm:tm() { StmtData::Tm { tm } }

        pub rule if_stmt() -> Vec<Stmt>
            = l:located(<if_stmt_data()>) { match l.data.1 {
                None => vec![Stmt::new(l.location.clone(), l.data.0)],
                Some(tm) => vec![Stmt::new(l.location.clone(), l.data.0), Stmt::new(tm.location.clone(), StmtData::Tm { tm })]
            } }
        pub rule if_stmt_data() -> (StmtData, Option<Tm>)
            = "if" _ branches:branch_list() { (StmtData::If { branches: branches.0 }, branches.1) }
        pub rule stmts_data() -> Vec<StmtData>
            = tm:tm() { vec![StmtData::Tm { tm }] }

        pub rule branch_list() -> (Vec<Branch>, Option<Tm>)
            = branch:branch() whitespace_except_newline() [',' | '\n']() _ rest:branch_list() { (vec![branch].into_iter().chain(rest.0).collect(), rest.1) }
            // last branch
            / branch:branch() { (vec![branch], None) }
            / tm:tm() { (vec![], Some(tm)) }
            / "" {(vec![], None)}

        pub rule branch() -> Branch = located(<branch_data()>)
        pub rule branch_data() -> BranchData
            = tm:tm() _ "=>" _ "{" _ stmts:stmt() _ "}" { BranchData::Bool { tm, stmts } }

        pub rule tm() -> Tm = located(<tm_data()>)
        #[cache_left_rec]
        pub rule tm_data() -> TmData
            = tm0:tm() _ "+" _ tm1:tm1() { TmData::Add { tm0: Rc::new(tm0), tm1: Rc::new(tm1) } }
            / tm:tm_data1() { tm }

        pub rule tm1() -> Tm = located(<tm_data1()>)
        #[cache_left_rec]
        rule tm_data1() -> TmData
            = tm0:tm1() _ "*" _ tm1:tm2() { TmData::Mul { tm0: Rc::new(tm0), tm1: Rc::new(tm1) } }
            / arg1:tm1() "." head:tm2() "(" _ args:list(<tm()>, <",">) _ ")" { TmData::FunCall { tm: Rc::new(head), args: [arg1].into_iter().chain(args.into_iter()).collect::<Vec<_>>() } }
            / head:tm2() "(" _ args:list(<tm()>, <",">) _ ")" { TmData::FunCall { tm: Rc::new(head), args } }
            / tm:tm1() _ "." _ name:name() { TmData::Proj { tm: Rc::new(tm), name } }
            / tm:tm_data2() { tm }

        pub rule tm2() -> Tm = located(<tm_data2()>)
        rule tm_data2() -> TmData
            = n:num() { TmData::Num { n } }
            / name:name() { TmData::Name { name } }
            / "(" _ tm:tm_data() _ ")" { tm }

        //

        rule num() -> i32
            = n:$(['0'..='9']+) { n.parse::<i32>().unwrap() }

        rule name() -> String
            = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*) { s.to_string() }

        //

        rule located<T>(tr: rule<T>) -> util::Located<T>
            = start:position!() t:tr() end:position!() { util::Located::new(util::Location::new(start, end), t) }

        rule list<T>(tr: rule<T>, sepr: rule<()>) -> Vec<T>
            = v:(t:tr() ** (_ sepr() _) {t}) (_ sepr() _)?
                { v }

        rule whitespace_sensitive_list<T>(tr: rule<T>, sepr: rule<()>) -> Vec<T>
            = v:(t:tr() ** sepr() {t}) sepr()?
                { v }

        //

        rule whitespace() = quiet!{[' ' | '\n' | '\t']*}
        rule whitespace_except_newline() = quiet!{ [' ' | '\t']* }
        rule _ = whitespace()
    }
}
