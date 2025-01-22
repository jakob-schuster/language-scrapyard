# elab_dep_recs

A language experiment.

Done:
- Binding names with `let x = false` or `let T = (Int, Int) -> Int`.
- Basic types `Bool`, `Int` and `Str`, with literals such as `false`, `10` and `'hello'`.
- Function types `(T1, T2 .. Tn) -> B`, and function literals `(a1 : T1, a2 : T2, .. an : Tn) : B => ..`. Functions take multiple arguments. Arguments and return types can all be either terms or types. Hence, the language is dependently typed. Functions are applied with parentheses `f(x)`.

To do:
- Record types `{ name = Str, age = Int }` with literals like `{ name = 'dan', age = 40 }`.

Examples:
```
let f = (x : Int) : Type => Str;
let g = (x : Int) : f(x) => 'hello';

g(10)
```