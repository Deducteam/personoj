# How to type check the output of Personoj

This guide shows you how to typecheck the output of Personoj
using [Lambdapi](https://github.com/Deducteam/lambdapi)
or [Dedukti](https://github.com/Deducteam/Dedukti).

## Preliminaries

First, you must tell `pp-dk` to write its output to a file.
To print the translation of the theory `xor_def` to the file `xor_def.lp`
(inside the read-eval-print-loop of PVS):
```lisp
* (get-theory "xor_def")
* (with-open-file (s "xor_def.lp" :direction :output)
    (pp-dk s * t))
```

## [Lambdapi](https://github.com/Deducteam/lambdapi)

Generate some mock files for Lambdapi:
```command
$ for f in booleans equalities notequal if_def boolean_props; do
      touch "${f}.lp"
	done
```

You can then check with Lambdapi that the file is correct:
```command
$ lamdapi check --map-dir pvs.prelude:. xor_def.lp
[...]
[.../xor_def.lp:15:210-218]
The proof is finished. Use 'end' instead.
[.../xor_def.lp:15:210-218]
Proof admitted.
axiom _ax0: Prf (∀ (λ A, ∀ (λ B, = (TL.double (XOR (TL.double A B)) (if A (λ _, ¬ B) (λ _, B))))))
[.../xor_def.lp:17:214-222]
Proof admitted.
```

*Note:* Lambdapi requires these mockfiles because each theory of the Prelude
imports all the theories defined before. The theory `xor_def` is the sixth
one defined in the Prelude.

## [Dedukti](https://github.com/Deducteam/Dedukti)

Some theories can be type checked with Dedukti once they have been refined by
Lambdapi.

To typecheck `xor_def.lp` with Dedukti:
```command
$ for f in booleans equalities notequal if_def boolean_props xor_def; do
      lambdapi export -o dk --map-dir pvs.prelude:. "$f".lp > pvs_prelude_"$f".dk
      dk check -I encoding.dk -e pvs_prelude_"$f".dk
	done
[SUCCESS] pvs_prelude_booleans.dk was successfully checked.
[SUCCESS] pvs_prelude_equalities.dk was successfully checked.
[SUCCESS] pvs_prelude_notequal.dk was successfully checked.
[SUCCESS] pvs_prelude_if_def.dk was successfully checked.
[SUCCESS] pvs_prelude_boolean_props.dk was successfully checked.
[.../xor_def.lp:15:210-218]
The proof is finished. Use 'end' instead.
[.../xor_def.lp:15:210-218]
Proof admitted.
[.../xor_def.lp:17:214-222]
Proof admitted.
[Warning] Trying to import the already loaded module personoj_coercions.
Unsupported pragma at position line:4 column:39: 'REQUIRE {|personoj_extra_arity-tools|}'
[Warning] Trying to import the already loaded module personoj_lhol.
[Warning] Trying to import the already loaded module personoj_logical.
[Warning] Trying to import the already loaded module personoj_nat.
[Warning] Trying to import the already loaded module personoj_pvs_cert.
[Warning] Trying to import the already loaded module personoj_telescope.
[SUCCESS] pvs_prelude_xor_def.dk was successfully checked.
```

<!-- TODO Simplify steps, add flags to dk and lambdapi invocations to have
     smaller messages -->
