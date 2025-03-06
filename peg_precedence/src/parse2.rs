use crate::surface::{Tm, TmData};
use crate::util;
use parser::tm;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

pub fn parse(string: &str) -> Result<Tm, ParseError> {
    tm(string).map_err(|e| ParseError {
        start: e.location.offset,
        end: e.location.offset + 1,
        message: "Parse error".to_string(),
    })
}

peg::parser! {
    grammar parser() for str {
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

        //

        rule whitespace() = quiet!{[' ' | '\n' | '\t']*}
        rule whitespace_except_newline() = quiet!{ [' ' | '\t']* }
        rule _ = whitespace()
    }
}
