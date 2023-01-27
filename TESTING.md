# How to run and add tests to Personoj

This guide shows you how to run the test suites of Personoj and how to add new
tests to these test suites.

## How to run the test suites

To run the unit tests:
```command
$ cd tests/thy_translation
$ pvs -raw -L test.lisp -E '(runall) (sb-ext:quit)'
```

and to translate and typecheck a portion of the Prelude:
```command
$ cd tests/prelude
$ pvs -raw -L test.lisp -E '(runall :without-proof-p t) (sb-ext:quit)'
```

## How to add tests

### Unit tests

If you have updated Personoj, you may have to correct some unit tests. To
update the test `eqtype`:
1. `cd tests/thy_translation`
2. Run the test:
   ```command
   $ pvs -raw -L test.lisp -E '(runtest "eqtype")'
   ```
3. Review the diff between the output of Personoj and the current `eqtype.lp.expected`
   that is displayed, and answer `y` to apply the displayed patch to
   `eqtype.lp.expected`, or `n` to do nothing.

If you want to add a new test:
1. `cd tests/thy_translation`
2. Write the PVS terms you want to translate in a new theory in `simple.pvs`
3. In `test.lisp`, add the name of your new theory to the list `*theories*`
4. Run the new test as described above, and apply the diff by answering `y`.

### Prelude translation

If a theory, say `real_defs` is disabled and you want to typecheck it,
1. Find its corresponding section in `theories.json`
   ```json
{
  "name": "real_defs",
	"disabled": true,
	...
}
	 ```
2. Switch `"disabled"` to `false` (or remove the line)
   ```json
{
  "name": "real_defs",
	"disabled": false,
	...
}
	 ```
