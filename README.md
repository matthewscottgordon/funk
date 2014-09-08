Funk
====

A pure functional language with strong static typing and eager (strict)
evaluation

At this point, Funk is primarily a vehicle for me (Matt Gordon) to
experiment with language design and learn about LLVM, but I do have a
specific (and hopefully useful) end-goal in mind. In a nutshell, Funk is
an ML-family language which borrows many features from Haskell. The syntax
is very similar to Haskell but, like other ML-family languages such as SML
and OCaml, it uses eager evaluation.

I have these main goals for Funk:

1.  It should be as easy as possible to reason about the *correctness*
    of a funk program.
2.  It should also be as easy as possible to reason about both the
    *time complexity* and *space complexity* of a program.
3.  Whenever possible, errors should be detected at compile time,
    rather than run time.
4.  It should compile to executable code which is fast and small.
5.  Eventually, it should be a practical language which I (and
    hopefully others) can use to write substantial applications.

In support of those goals, Funk has these main characteristics:

*   Pure Functional

    Referential transparency has huge advantages in the ease of reasoning
    about a program and verifying its correctness and referential
    transparency requires a purely functional programming style.

    In many ways, Funk's design goals are similar to those of the Rust
    language. Being purely functional is the major way Funk differs from
    Rust

*   Strong, Static Typing

    Strong, Static typing is necessary to provide the level of
    compile-time safety and correctness guarantees that Funk aims to
    have.

    Funk's type system is inspired by Haskell's with sum and product
    types, destructuring/pattern matching and ad-hoc typeclasses.

*   Strict Functions / Eager Evaluation

    Being non-strict, and therefore allowing lazy evaluation is a central
    design feature of Haskell and is the key way in which Funk differs
    from Haskell. Haskell's non-strictness has advantages and has lead
    to many great innovations but it can make it very difficult to
    reason about the time and space complexity of a program. It is
    confusing for beginners and makes it hard for even advanced
    programmers to optimize.



Building Notes
==============

* ghci will not work with ghc 4.6, due to issues with llvm-general.
* When building with ghc 4.8, llvm-general must be built with -fshared-llvm --enable-shared