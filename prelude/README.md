Prelude translation
===================

**Warning:** the makefile uses BSD-syntax, Linux users may use `bmake` rather than
`make`.

To translate all Prelude, assuming PVS sources have been downloaded to `<PVSPATH>`
``` sh
make PVSPATH=<PVSPATH>
```

Theories can be translated to lambdapi files, to translate theory "functions" of
Prelude,
`functions.lp`:

``` sh
make functions.lp
```

These files can then be type-checked. Lambdapi object files can be seen as the
result of type checking a file, hence `functions.lp` can be type-checked with
``` sh
make functions.lpo
```

The theories of the Prelude are registered in the file `theories`. In this file,
each line is a theory. One-line comments can be inserted with character `#`.
If a theory name is prefixed with a dash `-`, the theory is not translated nor
type-checked. Instead, an empty file is created with the name of the theory by.
This allows to translate and type check theories that are defined further in
the prelude, but do not depend on them, as theories of prelude require
(syntactically) all previous prelude theories.

For more details on the translation, refer to the user manual.
