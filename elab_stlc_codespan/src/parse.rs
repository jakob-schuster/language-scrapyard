use std::rc::Rc;

use peg::parser;

use crate::surface;

#[derive(Debug)]
pub struct ParseError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

pub fn parse(string: &str) -> Result<surface::Tm, ParseError> {
    parser::prog(string).map_err(|a| ParseError {
        start: a.location.offset,
        end: a.location.offset + 1,
        message: format!("Parser error: expected {}", a.expected),
    })
}

peg::parser! {
    grammar parser() for str {
        rule whitespace() = quiet!{[' ' | '\n' | '\t']*}
        rule whitespace_except_newline() = quiet!{[' ' | '\t']*}
        rule _ = whitespace()

        rule alphabetic() -> String
            = s:([ 'a'..='z' | 'A'..='Z'])
                { String::from(s) }
        rule alphanumeric() -> String
            = s:([ 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'])
                { String::from(s) }

        rule name() -> String
            = a:alphabetic()+ { a.join("") }
        rule binder() -> surface::Binder =
            start:position!() name:name() end:position!()
                { surface::Binder::new(surface::Location::new(start, end), name)}
        rule param() -> surface::Param =
                binder:binder() _ ":" _ ty:ty()? { surface::Param::new(binder, ty)}

        #[cache_left_rec]
        rule ty() -> surface::Ty
            = start:position!() data:ty_data() end:position!()
                    {surface::Ty::new(surface::Location::new(start, end), data)}
        #[cache_left_rec]
        rule ty_data() -> surface::TyData
            = name:name() {surface::TyData::Name(name)}
            / ty1:ty() _ "->" _ ty2:ty() {surface::TyData::FunType { ty1: Rc::new(ty1), ty2: Rc::new(ty2) }}

        rule bool_lit() -> bool = "true" {true} / "false" {false}
        rule int_lit() -> i32 = n:$(['0'..='9']+) { n.parse().unwrap() }

        rule op1() -> surface::Op1 =
            "-" {surface::Op1::Neg}
        rule op2() -> surface::Op2
            = "==" {surface::Op2::Eq}
            / "+" {surface::Op2::Add}
            / "-" {surface::Op2::Sub}
            / "*" {surface::Op2::Mul}

        pub rule prog() ->  surface::Tm = _ tm:tm() _ {tm}

        #[cache_left_rec]
        pub rule tm() -> surface::Tm
            = start:position!() data:tm_data() end:position!()
                {surface::Tm::new(surface::Location::new(start, end), data)}

        #[cache_left_rec]
        rule tm_data() -> surface::TmData = precedence!{
            fun:tm() _ arg:tm() {surface::TmData::App { fun: Rc::new(fun), arg: Rc::new(arg) }}
            --
            b:bool_lit() { surface::TmData::BoolLit { b }}
            i:int_lit() {surface::TmData::IntLit { i }}
            "let" _ binder:binder() _ params:param()**(_) _ ":" _ ty:ty()? _ "=" _ tm0:tm() _ ";" _ tm1:tm()
            {surface::TmData::Let {
                binder, params,
                ty,
                tm0: Rc::new(tm0), tm1: Rc::new(tm1)
            }}
            "if" _ guard:tm() _ "then" _ tm0:tm() _ "else" _ tm1:tm()
            {surface::TmData::IfThenElse {
                guard: Rc::new(guard), tm0: Rc::new(tm0), tm1: Rc::new(tm1)
            }}
            "fun" _ params:param()++(_) _ "=>" _ body:tm()
            { surface::TmData::FunLit { params, body: Rc::new(body) }}
            op:op1() _ tm:tm() { surface::TmData::Op1 {op, tm: Rc::new(tm)}}
            "op2" _ tm1:tm() _ op:op2() _ tm2:tm() {surface::TmData::Op2{op, tm1: Rc::new(tm1), tm2: Rc::new(tm2)}}
            tm:tm() _ ":" _ ty:ty() {surface::TmData::Ann { tm: Rc::new(tm), ty: Rc::new(ty) }}
            name:name() {surface::TmData::Name(name)}
        }

    }
}
