# language scrapyard 🛠️⚙️

Scrappy implementations of programming language ideas. Inspired by [brendan's language garden](https://github.com/brendanzab/language-garden)

In rough order of complexity:
- [math](./src/math.rs): interpreter for a simple math language
- [topl_imperative](./src/topl_imperative.rs): intepreter for an imperative language based on [Chapter 2 of Theories of Programming Languages by Reynolds](https://github.com/HotHat/books/blob/master/Theories%20of%20Programming%20Languages%3BREYNOLDS%3B2009.pdf)
- [stlc_bidirectional](./src/stlc_bidirectional.rs): bidirectional type checker and interpreter for the simply-typed lambda calculus, directly translated to Rust from [brendan's OCaml implementation](https://github.com/brendanzab/language-garden/tree/main/elab-stlc-bidirectional)
