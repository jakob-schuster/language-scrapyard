# language scrapyard 🛠️⚙️

Scrappy implementations of programming language ideas. Inspired by [brendan's language garden](https://github.com/brendanzab/language-garden)

In rough order of complexity:
- [math](./math): interpreter for a simple math language
- [topl_imperative](./topl_imperative): intepreter for an imperative language based on [Chapter 2 of Theories of Programming Languages by Reynolds](https://github.com/HotHat/books/blob/master/Theories%20of%20Programming%20Languages%3BREYNOLDS%3B2009.pdf)
- [elab_stlc_bidirectional](./elab_stlc_bidirectional): bidirectional type checker and interpreter for the simply-typed lambda calculus, directly translated to Rust from [brendan's OCaml implementation](https://github.com/brendanzab/language-garden/tree/main/elab-stlc-bidirectional)
- [elab_stlc_codespan](./elab_stlc_codespan): STLC with nice error messages.
- [elab_system_f_bidirectional](./elab_system_f_bidirectional): bidirectional type checker and interpreter for System F (stlc + type parameters), directly translated to Rust from [brendan's OCaml implementation](https://github.com/brendanzab/language-garden/tree/main/elab-system-f-bidirectional)
- [elab_dependent](./elab_dependent): bidirectional type checker and interpreter for a dependently typed language, directly translated to Rust from [brendan's OCaml implementation](https://github.com/brendanzab/language-garden/tree/main/elab-dependent)
- [pattern_matching](./pattern_matching): STLC with record types, and a first attempt at pattern matching.
- [elab_dep_recs](./elab_dep_recs): dependently-typed language experiment to support record types.