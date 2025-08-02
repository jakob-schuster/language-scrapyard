# STLC with error recovery

During elaboration, when a type error is encountered, that term gets elaborated to `Error` with the type `Unknown` and an error is added to the elaboration context. This means we catch multiple errors in one pass of the code, and instead of getting nothing from an erronious elaboration, we get a core program with errors.

Run your programs from `stdin`:
```
cat "let x = 4 in true" | cargo run
```

## Examples

### Passing programs

(Variables at the core level are represented as `#i`, where `i` is the index of the variable in the stack)

```
let a = 1 in a

(tm)> let a: Int = 1 in #0
(ty)> Int
```

```
if true then 0 else 1

(tm)> if true then 0 else 1
(ty)> Int
```

```
let f(x: Int) = x + 4 in f

(tm)> let f: Int -> Int = |x: Int| +[#0,#0] in #1
(ty)> Int -> Int
```

### Failing programs

```
x

(tm)> Error
(ty)> Unknown
(!!)> [0-1] unbound name 'x'
```

```
let x = y in z

(tm)> let x: Unknown = Error in Error
(ty)> Unknown
(!!)> [8-9] unbound name 'y'
(!!)> [13-14] unbound name 'z'
```

```
if true then 0 else false

(tm)> Error
(ty)> Unknown
(!!)> [0-25] mismatched branches of if expression
```

```
if 1 then 0 else false

(tm)> Error
(ty)> Unknown
(!!)> [3-4] mismatched types; Bool and Int
(!!)> [0-22] mismatched branches of if expression
```
