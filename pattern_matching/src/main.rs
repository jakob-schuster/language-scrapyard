mod core;
mod surface;
mod util;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{self, termcolor::StandardStream},
};
use lalrpop_util::lalrpop_mod;

lalrpop_util::lalrpop_mod!(pub parser);

#[derive(Debug)]
pub struct ParseError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

fn main() -> anyhow::Result<()> {
    let mut writer = StandardStream::stderr(term::termcolor::ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let code = "
        let a = 10;
        let b = 15;

        if b is
            'name is {r1} and {r2}' => name;
            n:_ => n;

        if rec is
            { seq = 'AGCTAGCTGATGCTAGTGCTG', id = '', .. } =>
            { age = 10 .. } =>

            ";

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
                .with_message(e.message)
                .with_labels(vec![Label::primary((), e.start..e.end)])
                .with_notes(vec![]);

            term::emit(&mut writer, &config, &file, &diagnostic);
        })
        .unwrap();

    let (ctm, cty) = surface::elab_infer(&surface::Context::default(), &tm)
        .map_err(|e| {
            let file = SimpleFile::new("<code>", code);
            let diagnostic = Diagnostic::error()
                .with_message(e.message)
                .with_labels(vec![Label::primary((), e.location.start..e.location.end)])
                .with_notes(vec![]);

            term::emit(&mut writer, &config, &file, &diagnostic);
        })
        .unwrap();
    let vtm = core::eval(&core::Env::default(), &ctm);

    println!("{}", vtm);
    Ok(())
}
