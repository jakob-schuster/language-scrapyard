mod core;
mod parse;
mod surface;

use lalrpop_util::lalrpop_mod;

// An implementation of a simply-typed lambda calculus (STLC) language,
// elaborated with bidirectional type checking,
// and good error reporting due to codespan.

lalrpop_util::lalrpop_mod!(pub parser);

fn main() -> anyhow::Result<()> {
    let mut writer = StandardStream::stderr(term::termcolor::ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let code = "
        let x (y : Int) : Int = - y;

        let g (x : Int) : Bool =
            if (x % 2) = 1 then true else false;

        g 4
    ";

    let a = parser::TmParser::new()
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

    // let a = parse::parse(code)
    //     .map_err(|e| {
    //         let file = SimpleFile::new("<code>", code);
    //         let diagnostic = Diagnostic::error()
    //             .with_message(e.message)
    //             .with_labels(vec![Label::primary((), e.start..e.end)])
    //             .with_notes(vec![]);

    //         term::emit(&mut writer, &config, &file, &diagnostic);
    //     })
    //     .unwrap();

    let ctx = surface::Context::default();

    let (ctm, cty) = surface::elab_infer(&ctx, &a)
        .map_err(|e| {
            let file = SimpleFile::new("<code>", code);
            let diagnostic = Diagnostic::error()
                .with_message(e.message)
                .with_labels(vec![Label::primary((), e.location.start..e.location.end)])
                .with_notes(vec![]);

            term::emit(&mut writer, &config, &file, &diagnostic);
        })
        .unwrap();

    let env = core::Env::default();
    let vtm = core::eval(env, ctm);

    println!("{vtm}");

    Ok(())
}

use clap::{Parser, Subcommand};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{self, termcolor::StandardStream},
};
use parse::ParseError;

#[derive(Debug, Parser)]
struct Cli {
    /// A script to parse.
    #[arg(short, long)]
    script: String,
}
