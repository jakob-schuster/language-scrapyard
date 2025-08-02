use parse::parse;

mod parse;
mod surface;
mod util;

fn main() {
    let code = "
        read.seq.name() + 10 * 5;

        if  a => {c}, v => {z}

        a";

    let prog = parse(code).unwrap();

    println!("{:?}", prog);
}
