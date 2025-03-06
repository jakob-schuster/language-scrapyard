use lalrpop_util::lalrpop_mod;

mod surface;
mod util;

lalrpop_util::lalrpop_mod!(pub parser);

fn main() {
    let code = r"
        a = 10
        b = true
        ? a
    ";

    let prog = parser::ProgParser::new().parse(code);

    println!("{:?}", prog);
}
