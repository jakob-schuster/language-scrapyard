#[cfg(test)]
use crate::fully_eval;

#[test]
fn readme1() {
    insta::assert_snapshot!(fully_eval("
            let f = (x : Int) : Type => Str;
            let g = (x : Int) : f(x) => 'hello';

            g(10)
        ").unwrap(), @"'hello'")
}

#[test]
fn readme2() {
    insta::assert_snapshot!(fully_eval("
            let rec = { name = 'dan', age = 40 };
            let nameof = ( rec : { name : Str, age : Int } ) : Str => rec.name;
            nameof(rec)
        ").unwrap(), @"'dan'")
}
#[test]
fn readme3() {
    insta::assert_snapshot!(fully_eval("
            let not = (x : Bool) : Bool => if x is true => false | false => true;
            let and = (x : Bool, y : Bool) : Bool => if x is true => y | false => false;
            and(true, not(not(true)))
        ").unwrap(), @"true")
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
fn dependent_fail1() {
    insta::assert_snapshot!(format!("{:?}", fully_eval("
            let f = (x : Int) : Type => Int;
            let g = (x : Int) : f(x) => 'hello';

            g(10)
        ")), @r#"Err(ElabError(ElabError { location: Location { start: 108, end: 113 }, message: "mismatched types: expected Str, found Int" }))"#)
}

#[test]
fn dependent_fail2() {
    insta::assert_snapshot!(format!("{:?}", fully_eval("
            let f = (x : Int) : Type => if x is 0 => Str | _ => Int;
            let g = (x : Int) : f(x) => 'hello';

            g(10)
        ")), @r#"Err(ElabError(ElabError { location: Location { start: 132, end: 137 }, message: "mismatched types: expected Str, found Int" }))"#)
}

#[test]
fn dependent() {
    insta::assert_snapshot!(format!("{:?}", fully_eval("
            let f = (x : Int) : Type => if x is 0 => Str | _ => Int;
            let g = (x : Int) : f(x) => 'hello';

            g(0)
        ")), @r#"Ok("'hello'")"#)
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
fn rec_proj1() {
    insta::assert_snapshot!(fully_eval("
            let f = { name = 'hello' };
            f.name
        ").unwrap(), @"'hello'")
}

#[test]
fn rec_proj2() {
    insta::assert_snapshot!(
        format!("{:?}", fully_eval("
                let f = () : { name : Str } => { name = 'hello' };
                {f()}.name
            ")),
        @r#"Ok("'hello'")"#
    )
}
#[test]
fn rec_proj3() {
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
fn rec_proj_fail1() {
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

#[test]
fn pattern1() {
    insta::assert_snapshot!(
        format!("{:?}", fully_eval("
                let f = (x : Bool) : Int => if x is true => 1 | false => 0;
                f(true)
            ")),
        @r#"Ok("1")"#
    )
}

#[test]
fn pattern2() {
    insta::assert_snapshot!(
        format!("{:?}", fully_eval("
                let not = (x : Bool) : Bool => if x is true => false | false => true;
                not(true)
            ")),
        @r#"Ok("false")"#
    )
}
