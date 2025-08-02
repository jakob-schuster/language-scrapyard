use chumsky::{text, Parser};

use crate::surface::Prog;

pub struct ParseError {}

fn parse(code: &str) -> Result<Prog, ParseError> {
    let num = text::int(10).to_span().map(|s| s);

    todo!()
}
