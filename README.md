HaScalaM
=========

It converts (textually) to and fro between `Haskell` and `Scalameta` `AST`s.

Use a script to convert from `Scalameta` "`Source`" string to mirroring `Haskell`
classes, types and instances. Use the `Show` instances to convert from mirrored
`Haskell` types to `Scalameta` string.

It is fairly complete as far as `Scalameta` coverage is concerned.
It starts with "constraint" classes corresponding to `Scalameta` traits,
goes through types corresponding to OOP, and ends with wrapper types that
have `Show` instances.

The disadvantage is that constructors are "doubled", e.g., "`TDo (SmDoT`" is
required, because "`TDo`" involves type equality, while "`SmDoT`" only uses type
constraints. ("`T`" stands for "`term`".)

It is used [here](https://github.com/sjbiaga/pisc-dotarrow) at the
metaprogramming level to convert to and fro between `Scala` and `Haskell`.
