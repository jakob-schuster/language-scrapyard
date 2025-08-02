use std::{fmt::Debug, rc::Rc};

use crate::{
    parse::parser::tm,
    surface::{InfixOp, Param, PrefixOp, Span, Spanned, Tm, TmData, Ty, TyData},
};

pub struct ParseError {
    pos: usize,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("Parse Error at {}", self.pos).fmt(f)
    }
}

pub fn parse(string: &str) -> Result<Tm, ParseError> {
    tm(string).map_err(|e| ParseError {
        pos: e.location.offset,
    })
}

peg::parser! {
    grammar parser() for str {

        pub rule tm() -> Tm = spanned(<tm_data()>)
        #[cache_left_rec]
        rule tm_data() -> TmData =
            "let" _ def_name:spanned(<name()>) _ "(" _ params:list(<param()>,<",">) _ ")" _ def_body_ty:(":" _ ty:ty() { ty})? _ "=" _ def_body:tm() _ "in" _ body:tm() {
                TmData::Let { def_name, params, def_body_ty, def_body: Rc::new(def_body), body: Rc::new(body) }
            } /
            "let" _ def_name:spanned(<name()>) _ def_body_ty:(":" _ ty:ty() { ty})? _ "=" _ def_body:tm() _ "in" _ body:tm() {
                TmData::Let { def_name, params: vec![], def_body_ty, def_body: Rc::new(def_body), body: Rc::new(body) }
            } /
            "if" _ head:tm() _ "then" _ tm1:tm() _ "else" _ tm2:tm() {
                TmData::IfThenElse { head: Rc::new(head), tm1: Rc::new(tm1), tm2: Rc::new(tm2) }
            } /

            "|" _ params:list(<param()>,<",">) _ "|" _ body:tm() {
                TmData::FunLit { params, body: Rc::new(body) }
            } /

            head:tm() _ "into" _ body:tm() { TmData::App { head: Rc::new(head), arg: Rc::new(body) } } /
            tm1:tm() _ op:infix_op() _ tm2:tm() {
                TmData::Infix { op, tm1: Rc::new(tm1), tm2: Rc::new(tm2) }
            } /
            op:prefix_op() _ tm:tm() {
                TmData::Prefix { op, tm: Rc::new(tm) }
            } /

            b:bool_val() { TmData::BoolLit { b } } /
            i:num_val() { TmData::IntLit { i } } /
            name:name() { TmData::Name { name } } /
            "(" _ tm_data:tm_data() _ ")" { tm_data }

        rule ty() -> Ty = spanned(<ty_data()>)
        #[cache_left_rec]
        rule ty_data() -> TyData =
            head_ty:ty() _ "->" _ body_ty:ty() {
                TyData::FunTy { head_ty: Rc::new(head_ty), body_ty: Rc::new(body_ty) }
            } /
            name:name() { TyData::Name { name } }

        //

        rule param() -> Param =
            binder:spanned(<name()>) _ ty:(":" _ ty:ty() { ty })? {
                Param { binder, ty }
            }

        //

        rule prefix_op() -> PrefixOp =
            "-" { PrefixOp::Neg }
        rule infix_op() -> InfixOp =
            "==" {InfixOp::Eq} /
            "+" {InfixOp::Add} /
            "-" {InfixOp::Sub} /
            "*" {InfixOp::Mul}

        rule bool_val() -> bool
            = "true" { true }
            / "false" { false }
        rule num_val() -> i32
            = n:$(['0'..='9']+) { n.parse::<i32>().unwrap() }

        //

        rule name() -> String
            = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '0'..='9' | '!']*) { s.to_string() }

        //

        rule spanned<T>(tr: rule<T>) -> Spanned<T> =
            start:position!() t:tr() end:position!() {
                Spanned::new(Span::new(start, end), t)
            }

        //

        rule list<T>(tr: rule<T>, sepr: rule<()>) -> Vec<T> =
            v:(t:tr() ** (_ sepr() _) {t}) (_ sepr() _)? { v }


        //

        rule whitespace() = quiet!{[' ' | '\n' | '\t']*}
        rule whitespace_except_newline() = quiet!{ [' ' | '\t']* }
        rule _
            = whitespace() "#" [c if c != '\n']* "\n" _()
            / whitespace()
    }
}
