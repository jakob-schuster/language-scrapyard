use crate::surface::Tm;
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
        #[cache_left_rec]
        pub rule tm() -> Tm = precedence!{

            tm0:(@) _ "+" _ tm1:@ { Tm::Add { tm0: Rc::new(tm0), tm1: Rc::new(tm1) } }
            --
            tm0:(@) _ "*" _ tm1:@ { Tm::Mul { tm0: Rc::new(tm0), tm1: Rc::new(tm1) } }
            --
            arg1:(@) "." head:tm() "(" _ args:list(<tm()>, <",">) _ ")" { Tm::FunCall { tm: Rc::new(head), args: [arg1].into_iter().chain(args.into_iter()).collect::<Vec<_>>() } }
            tm:(@) _ "." _ name:name() { Tm::Proj { tm: Rc::new(tm), name } }
            --
            n:num() { Tm::Num { n } }
            name:name() { Tm::Name { name } }
            "(" _ tm:tm() _ ")" { tm }
        }
        //

        rule num() -> i32
            = n:$(['0'..='9']+) { n.parse::<i32>().unwrap() }

        rule name() -> String
            = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*) { s.to_string() }

        //

        rule list<T>(tr: rule<T>, sepr: rule<()>) -> Vec<T>
            = v:(t:tr() ** (_ sepr() _) {t}) (_ sepr() _)?
                { v }

        //

        rule whitespace() = quiet!{[' ' | '\n' | '\t']*}
        rule whitespace_except_newline() = quiet!{ [' ' | '\t']* }
        rule _ = whitespace()
    }
}
