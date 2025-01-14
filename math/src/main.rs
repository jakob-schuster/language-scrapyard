use lalrpop_util::lalrpop_mod;

mod loc_math;
mod math;

lalrpop_mod!(pub loc_parser);

fn main() {
    println!(
        "{:?}",
        loc_parser::ExpParser::new().parse("10 + 1 * 10 + 4")
    );
}
