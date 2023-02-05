# [Personoj](https://github.com/Deducteam/personoj)

Copyright [Deducteam](https://deducteam.gitlabpages.inria.fr) 2021-2023

With *Personoj* you can translate [PVS](http://pvs.csl.sri.com) specifications to
[Lambdapi](https://github.com/Deducteam/lambdapi).
It is similar in purpose to
[Agda2Dedukti](https://github.com/Deducteam/Agda2Dedukti),
[HOLLightToDk](https://github.com/Deducteam/HOLLightToDk)
or [CoqInE](https://github.com/Deducteam/CoqInE).

You are free to copy, modify and distribute Personoj with attribution under the
terms of the CeCILL-B license. See the `LICENSE` file for details.

## Getting started

Before using Personoj, you need:

- [PVS 7.1](https://pvs.csl.sri.com/downloads.html) built with SBCL
- BSD make
- [lambdapi](https://github.com/gabrielhdt/lambdapi)
  [branch `coercions`](https://opam.ocaml.org/doc/Usage.html#opam-pin)

To install Personoj, load `tools/personoj.lisp` with any ANSI Common Lisp
interpreter. PVS can be used for that:
```command
$ pvs -raw -L tools/personoj.lisp
```

Here is a one-liner that translates the theory `booleans` from the prelude of
PVS:
```command
$ pvs -raw -E '(pp-dk *standard-output* (get-theory "booleans") t)' -E '(uiop:quit)' 2> /dev/null
require open personoj.lhol personoj.logical personoj.pvs_cert
personoj.eq personoj.restrict personoj.coercions;
require personoj.telescope as TL;
require personoj.extra.arity-tools as A;
require open personoj.nat;
require open personoj.cast;
// Theory booleans
constant symbol prop: Set;

symbol prop: Set ≔ prop begin admitted;

constant symbol false: El prop begin admitted;

constant symbol true: El prop begin admitted;

constant symbol NOT: El (prop ~> prop) begin admitted;

constant symbol ¬: El (prop ~> prop) begin admitted;

constant symbol AND: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol &: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol ∧: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol OR: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol ∨: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol IMPLIES: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol =>: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol ⇒: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol WHEN: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol IFF: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol <=>: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

constant symbol ⇔: El ((TL.code (TL.double! prop prop)) ~> prop) begin admitted;

```

## More documentation

You can ask PVS to show the documentation of any function using the Lisp
function `describe` (e.g. try `(describe 'describe)`).

The file [`CHECKING.md`](./CHECKING.md) describes how to type check with
Lambdapi or Dedukti the output of Personoj.

The file [`TESTING.md`](./TESTING.md) shows how to run, add or update the tests
of Personoj.

The file [`HACKING.md`](./HACKING.md) describes some internals of Personoj that
are relevant for developers.

A list of missing features is available in the comments of the file
[`pp-dk3.lisp`](./pvs_patches/pvs2dk/pp-dk3.lisp).

## Contact

If you found a bug, you can report it [there](https://github.com/Deducteam/personoj/issues).

You can also ask for help, start a discussion, introduce yourself or say
hello at `dedukti-dev /at/ inria \dot\ fr`.
