# Personoj -- Expressing PVS in Dedukti

*Personoj* transpiles [PVS](http://pvs.csl.sri.com) to
[Dedukti](https://deducteam.github.io).

PVS is a highly automated higher order proof environment based on Simple
Type Theory featuring predicate subtyping (also called subset types or
refinement types). Dedukti is a language with its specification as well
as one of its implementations. It is a logical framework. Personoj does
not target the implementation
[Dedukti](https://github.com/Deducteam/dedukti), but
[*lambdapi*](https://github.com/Deducteam/lambdapi).

The repository contains
- lambdapi files to encode the logic of PVS into Dedukti
- lisp files that implement a translation from PVS theories to Dedukti
  files.

## Encoding (`encoding/`)

The main modules are:

- *lhol:* Simple Type Theory,
- *pvs_cert:* predicate subtyping,
- *logical:* logical connectives,
- *telescope:* encoding of telescopes (or dependent tuples),
- *eq:* encoding of propositional equality,

Modules in *examples* provide some developments, *examples.stack* may
be the most interesting one.
Modules in *alt* provide some alternative definitions.

Encoding is checked regularly using the continuous
integration of github. The configuration file for the CI is
`.github/workflows/check_encoding.yml`.

## Transpiler code (`pvs_patches/`)

The code for the transpiler is written in [Common
Lisp](https://common-lisp.net). The code must be loaded in a PVS
process. A helper function defined in `tools/load-personoj.lisp` can be
placed in `~/.pvs.lisp` to load easily those files from PVS.

The transpiler is defined by the function `PVS::PP-DK`,

```lisp
(defun pp-dk (stream x &optional without-proofs)
  "Print PVS object X on STREAM. Proofs are not exported if WITHOUT-PROOFS
is true."
  ...)
```

## Prelude translation (`tests/prelude/`)

The prelude may be translated using the lisp function `runtest` defined
in `test.lisp`.  PVS theories that do not typecheck (yet) are translated
to empty files.
All theories of prelude may be translated and typechecked at
once using the function `runall`. The `runall` function takes a
[JSON](https://www.json.org) file that specifies which theories can be
typechecked and which one cannot. The fields of that file are described
in the documentation of `theory-select`.

The translation is run without exporting proofs and the
output is type checked by the continuous integration in
`.github/workflows/pvs_prelude.yml`.

## Install

Dependencies:
- [PVS 7.1](https://pvs.csl.sri.com/downloads.html)
- [lambdapi](https://github.com/gabrielhdt/lambdapi) branch `coercions`

To run PVS with personoj, copy the content of `tools/load-personoj.lisp`
in `~/.pvs.lisp` and either

- add the line
  ```lisp
  (load-personoj "personojpath")
  ```
  where `personojpath` is the path to the root of the local copy of this
  repository after the definition of `load-personoj` in `~/.pvs.lisp`,

- set the environment  `PERSONOJPATH` to the root of the local copy of
  this repository.
