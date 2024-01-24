# How to type check the output of Personoj

This guide shows you how to typecheck the translation of the theory `xor_def`
from the prelude of PVS using [Lambdapi](https://github.com/Deducteam/lambdapi)
or [Dedukti](https://github.com/Deducteam/Dedukti).

It is assumed you have installed lambdapi and PVS as indicated in the
[readme](./README.md).

In code snippets, `*` denotes the Lisp prompt and `$` the shell prompt.

## Preliminaries

First, write the translation of the theory to a file, say `xor_def.lp`:
```lisp
* (with-open-file (s "xor_def.lp" :direction :output)
    (pp-lp s (get-theory "xor_def") t))

NIL
```

## Typecheck with [Lambdapi](https://github.com/Deducteam/lambdapi)

Generate some mock files for Lambdapi:
```command
$ for f in booleans equalities notequal if_def boolean_props; do
      touch "${f}.lp"
  done
```

You can then check with Lambdapi that the file is well-formed:
```command
$ lamdapi check --map-dir pvs.prelude:. --no-warnings --verbose 0 xor_def.lp
```

*Note:* Lambdapi requires these mockfiles because each theory of the prelude
imports all the theories defined before.
The first five theories are redundant with the encoding provided with personoj,
so we replace them by empty files so that import doesn't fail.
The theory `xor_def` is the sixth one defined in the prelude.

## Typecheck with [Dedukti](https://github.com/Deducteam/Dedukti)

To typecheck the theory `xor_def` with Dedukti:

1. Translate the Lambdapi files to Dedukti:
   ```command
   $ for f in booleans equalities notequal if_def boolean_props xor_def; do
         lambdapi export --no-warnings --verbose 0 --output dk \
             --map-dir pvs.prelude:. "$f".lp > pvs_prelude_"$f".dk
     done
   ```

2. Install [Dedukti](https://github.com/Deducteam/Dedukti).

2. Generate the object files of the encoding:
   for that, assuming `DKENCODING` is a variable that contains the path to the
   directory `encoding.dk`,
   ```command
   $ cd "$DKENCODING"
   $ bmake
   dk check --quiet -e personoj_lhol.dk
   dk check --quiet -e personoj_logical.dk
   dk check --quiet -e personoj_pvs_cert.dk
   dk check --quiet -e personoj_nat.dk
   dk check --quiet -e personoj_extra_arity-tools.dk
   dk check --quiet -e personoj_coercions.dk
   dk check --quiet -e personoj_telescope.dk
   dk check --quiet -e personoj_eq.dk
   dk check --quiet -e personoj_restrict.dk
   dk check --quiet -e personoj_cast.dk
   ```

3. Finally, type check the files ending with `.dk` with Dedukti:
   ```command
   $ for f in booleans equalities notequal if_def boolean_props xor_def; do
         dk check --quiet --include "$DKENCODING" -e pvs_prelude_"$f".dk
     done
   ```
