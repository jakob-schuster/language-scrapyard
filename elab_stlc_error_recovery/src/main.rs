use crate::{parse::parse, surface::elab};

mod core;
mod parse;
mod prim;
mod surface;
mod util;

fn main() {
    let tm = parse("let a(x: Int,y: Bool) = (x + 2) in (a into 10) into false").unwrap();
    let (core_tm, core_ty, ctx) = elab(&tm);

    println!("{}", core_tm);
    println!("{}", core_ty);
    println!(
        "{}",
        ctx.errors
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(",")
    )
}

mod test {
    #[cfg(test)]
    fn parse_and_elab(code: &str) -> String {
        use crate::{parse::parse, surface::elab};

        let tm = parse(code).unwrap();
        let (core_tm, core_ty, ctx) = elab(&tm);

        format!(
            "{}\n{}\n{}",
            core_tm,
            core_ty,
            ctx.errors
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }

    #[test]
    fn intlit_pass1() {
        insta::assert_snapshot!(parse_and_elab("1"), r"")
    }
    #[test]
    fn let_pass1() {
        insta::assert_snapshot!(parse_and_elab("let a = 1 in a"), r"")
    }
    #[test]
    fn app_pass1() {
        insta::assert_snapshot!(parse_and_elab("let a(x: Bool) = 1 in a into false"), r"")
    }
    #[test]
    fn app_pass2() {
        insta::assert_snapshot!(
            parse_and_elab("let a(x: Bool,y: Int) = 1 in a into false"),
            r""
        )
    }
    #[test]
    fn app_pass3() {
        insta::assert_snapshot!(
            parse_and_elab("let a(x: Bool,y: Int) = 1 in (a into false) into 10"),
            r""
        )
    }
    #[test]
    fn if_pass1() {
        insta::assert_snapshot!(parse_and_elab("if true then 1 else 0"), r"")
    }
    #[test]
    fn if_fail1() {
        insta::assert_snapshot!(parse_and_elab("if true then 1 else false"), r"")
    }
    #[test]
    fn if_fail2() {
        insta::assert_snapshot!(parse_and_elab("if 0 then true else false"), r"")
    }
    #[test]
    fn if_fail3() {
        insta::assert_snapshot!(parse_and_elab("if 0 then 1 else false"), r"")
    }
    #[test]
    fn name_fail1() {
        insta::assert_snapshot!(parse_and_elab("b"), r"")
    }
}
