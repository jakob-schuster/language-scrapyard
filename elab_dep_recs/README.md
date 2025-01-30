# elab_dep_recs

A language experiment.

Done:
- Binding names with `let x = false` or `let T = (Int, Int) -> Int`.
- Basic types `Bool`, `Int` and `Str`, with literals such as `false`, `10` and `'hello'`.
- Function types `(T1, T2 .. Tn) -> B`, and function literals `(a1 : T1, a2 : T2, .. an : Tn) : B => ..`. Functions take multiple arguments. Arguments and return types can all be either terms or types. Hence, the language is dependently typed. Functions are applied with parentheses `f(x)`.
- Record types `{ name : Str, age : Int }` with literals like `{ name = 'dan', age = 40 }`.
- Pattern matching, with syntax like `if x is true => .. | false => ..`
- Foreign functions (sort of... it's a library of hardcoded Rust functions that you can wrap, declaring their argument and return types)

Examples:
```
let f = (x : Int) : Type => Str;
let g = (x : Int) : f(x) => 'hello';

g(10)
```

```
let rec = { name = 'dan', age = 40 };
let nameof = ( rec : { name : Str, age : Int } ) : Str => rec.name;
nameof(rec)
```

```
let not = (x : Bool) : Bool => if x is
      true => false
    | false => true;

let and = (x : Bool, y : Bool) : Bool => if x is
      true => y
    | false => false;

and(true, not(not(true)))
```

Note that this will work:
```
let f = (x : Int) : Type => if x is 0 => Str | _ => Int;
let g = (x : Int) : f(x) => 'hello';

g(0)
```

But this will produce an error:
```
let f = (x : Int) : Type => if x is 0 => Str | _ => Int;
let g = (x : Int) : f(x) => 'hello';

g(1)
```