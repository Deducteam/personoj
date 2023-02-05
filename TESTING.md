# How to run and add tests to Personoj

This guide shows you how to run the test suites of Personoj and how to add new
tests to these test suites.

## How to run the test suites

Run the unit tests:
```command
$ cd tests/unit_tests
$ pvs -raw -L test.lisp -E '(runall) (uiop:quit)'
```

Translate and typecheck a portion of the prelude of PVS
(requires Lambdapi):
```command
$ cd tests/prelude
$ pvs -raw -L test.lisp -E '(runall :without-proof-p t) (uiop:quit)'
```

## How to add tests

### Unit tests

If you have updated Personoj, you may have to correct some unit tests. To
update the test, say, `eqtype`:
1. `cd tests/unit_tests`
2. Run the test:
   ```command
   $ pvs -raw -L test.lisp -E '(runtest "eqtype")'
   ```
3. Review the diff between the output of Personoj and the current `eqtype.lp.expected`
   that is displayed. Answer `y` to apply the displayed patch to
   `eqtype.lp.expected`, or `n` to do nothing.

If you want to add a new test:
1. `cd tests/unit_tests/`
2. Write some PVS you want to translate in a new theory inside the file
   `simple.pvs`
3. In `test.lisp`, add the name of your new theory to the list `*theories*`.
4. Run the new test as described above, and apply the diff by answering `y`.

### Translation of the prelude

If a theory, say `real_defs` is not translated when running the prelude test
and you want to test it,
1. Find its corresponding section in `theories.json`
   ```json
   {
		 "name": "real_defs",
		 "disabled": true,
		 "comments": "...",
	 }
	 ```
2. Switch `"disabled"` from `true` to `false` (or remove the line)
   ```json
   {
		 "name": "real_defs",
		 "disabled": false,
		 "comments": "...",
   }
	 ```

For more information about the file `theories.json`,
see the documentation of
[`tests/prelude/test.lisp`](./tests/prelude/test.lisp).
