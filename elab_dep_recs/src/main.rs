// A dependently-typed language, with records, and some built-in functions.
// Functions can fail dynamically.

mod core;
mod surface;
mod util;

use core::EvalError;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{self, termcolor::StandardStream},
};
use lalrpop_util::lalrpop_mod;
use surface::ElabError;

lalrpop_util::lalrpop_mod!(pub parser);

#[derive(Debug)]
pub enum Error {
    ParseError(ParseError),
    ElabError(ElabError),
    EvalError(EvalError),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

fn main() {
    let mut writer = StandardStream::stderr(term::termcolor::ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let code = "
        let a = (t1 : Type, t2 : Type) : Type => (t1, t1) -> t2;
        let b = (t : Type) : Type => a(t, t);

        b(b(Int))
    ";

    println!("{}", fully_eval(code).unwrap());
}

fn fully_eval(code: &str) -> Result<String, Error> {
    let mut writer = StandardStream::stderr(term::termcolor::ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let tm = parser::TmParser::new()
        .parse(code)
        .map_err(|e| match e {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError {
                start: location,
                end: location,
                message: "invalid token".to_string(),
            },
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => ParseError {
                start: location,
                end: location,
                message: format!("unrecognized EOF! expected: {}", expected.join(" ")),
            },
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => ParseError {
                start: token.0,
                end: token.2,
                message: format!("unrecognized token, expected: {}", expected.join(" ")),
            },
            lalrpop_util::ParseError::ExtraToken { token } => ParseError {
                start: token.0,
                end: token.2,
                message: "unexpected token".to_string(),
            },
            lalrpop_util::ParseError::User { error } => ParseError {
                start: 0,
                end: code.len(),
                message: error.to_string(),
            },
        })
        .map_err(|e| {
            let file = SimpleFile::new("<code>", code);
            let diagnostic = Diagnostic::error()
                .with_message(e.clone().message)
                .with_labels(vec![Label::primary((), e.start..e.end)])
                .with_notes(vec![]);

            term::emit(&mut writer, &config, &file, &diagnostic);

            Error::ParseError(e)
        })?;

    let (ctm, cty) = surface::infer(&surface::Context::standard_library(), &tm).map_err(|e| {
        let file = SimpleFile::new("<code>", code);
        let diagnostic = Diagnostic::error()
            .with_message(e.clone().message)
            .with_labels(vec![Label::primary((), e.location.start..e.location.end)])
            .with_notes(vec![]);

        term::emit(&mut writer, &config, &file, &diagnostic);

        Error::ElabError(e)
    })?;
    let vtm = core::eval(&surface::Context::standard_library().tms, &ctm)
        .map_err(|e| Error::EvalError(e))?;

    Ok(vtm.to_string())
}

#[cfg(test)]
mod test {
    use crate::fully_eval;

    #[test]
    fn readme_example() {
        insta::assert_snapshot!(fully_eval("
            let f = (x : Int) : Type => Str;
            let g = (x : Int) : f(x) => 'hello';

            g(10)
        ").unwrap(), @"'hello'")
    }

    #[test]
    fn fun_app() {
        insta::assert_snapshot!(fully_eval("let a = (x : Int) : Int => x; a(10)").unwrap(), @"10")
    }

    #[test]
    fn nested_fun_app() {
        let code = "
            let a = (t1 : Type, t2 : Type) : Type => (t1, t1) -> t2;
            let b = (t : Type) : Type => a(t, t);

            b(b(Int))
        ";

        let result = fully_eval(code).unwrap();
        insta::assert_snapshot!(result, @"((Int, Int) -> Int, (Int, Int) -> Int) -> (Int, Int) -> Int");
    }

    #[test]
    fn rec_ty() {
        insta::assert_snapshot!(fully_eval("
            let f = { name: Str };
            f
        ").unwrap(), @"{ name = Str }")
    }

    #[test]
    fn rec_lit() {
        insta::assert_snapshot!(fully_eval("
            let f = { name = 'hello' };
            f
        ").unwrap(), @"{ name = 'hello' }")
    }

    #[test]
    fn rec_proj() {
        insta::assert_snapshot!(fully_eval("
            let f = { name = 'hello' };
            f.name
        ").unwrap(), @"'hello'")
    }

    #[test]
    fn rec_proj_2() {
        insta::assert_snapshot!(
            format!("{:?}", fully_eval("
                let f = () : { name : Str } => { name = 'hello' };
                {f()}.name
            ")),
            @r#"Ok("'hello'")"#
        )
    }
    #[test]
    fn rec_proj_fail() {
        insta::assert_snapshot!(
            format!("{:?}", fully_eval("
                let f = () : { name : Int } => { name = 'hello' };
                {f()}.name
            ")),
            @r#"Err(ElabError(ElabError { location: Location { start: 48, end: 66 }, message: "mismatched types: expected { name = Int }, found { name = Str }" }))"#
        )
    }

    #[test]
    fn rec_proj_fail2() {
        insta::assert_snapshot!(
            format!("{:?}", fully_eval("
                let f = () : { name : Str } => { name = 'hello' };
                {f()}.age
            ")),
            @r#"Err(ElabError(ElabError { location: Location { start: 84, end: 89 }, message: "trying to access non-existent field" }))"#
        )
    }

    #[test]
    fn rec_proj_3() {
        insta::assert_snapshot!(
            format!("{:?}", fully_eval("
                let f = () : { name : Str } => { name = 'hello' };
                let g = (name : Str) : Int => 1;
                g({f()}.name)
            ")),
            @r#"Ok("1")"#
        )
    }
    #[test]
    fn rec_proj_fail3() {
        insta::assert_snapshot!(
            format!("{:?}", fully_eval("
                let f = () : { name : Str } => { name = 'hello' };
                let g = (age : Int ) : Int => 1;
                g({f()}.name)
            ")),
            @r#"Err(ElabError(ElabError { location: Location { start: 135, end: 145 }, message: "mismatched types: expected Int, found Str" }))"#
        )
    }
}
