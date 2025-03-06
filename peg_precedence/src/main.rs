use parse2::parse;

mod parse2;
mod surface;
mod util;

fn main() {
    let code = "read.seq.name() + 10 * 5";

    let tm = parse(code).unwrap();

    println!("{:?}", tm);
}
