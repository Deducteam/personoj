# Personoj developer guide

This files provides an overview of the repository and some technical details.

1. [Repository architecture](#repository-architecture)
2. [Continuous integration](#continuous-integration)
3. [Why is the output of Personoj checked with a fork of Lambdapi?](#why-is-the-output-of-personoj-checked-with-a-fork-of-Lambdapi?)
   1. [Lambdapi can't apply coercions with implicit arguments](#lambdapi-cant-apply-coercions-with-implicit-arguments)
	 2. [Coercions may create unsafe symbols](#coercions-may-create-unsafe-symbols)

## Repository architecture

- [`encoding`](encoding/): encoding of PVS in Lambdapi
- [`encoding.dk`](encoding.dk/): encoding of PVS in Dedukti
- [`mk`](mk/): recipes to be used by BSD Make
- [`pvs_patches`](pvs_patches/): source code of personoj
- [`tests`](tests/): unit tests and procedures to translate and type check the Prelude of
	PVS
- [`tools`](tools/): scripts for the continuous integration and other tools

## Continuous integration

The continuous integration checks that the encodings in `encoding/` are
accepted by lambdapi. It also runs the tests in `tests/`.

## Why is the output of Personoj checked with a fork of Lambdapi?

The output of Personoj can only be typechecked wih a [fork of
Lambdapi](https://github.com/gabrielhdt/lambdapi/tree/coercions)
which solves two issues with the main version.

### Lambdapi can't apply coercions with implicit arguments

The main version of Lambdapi often fails to apply coercions in presence of
implicit arguments. Coercion rules must filter types to be applied.
Therefore, if a type is an existential variable, it won't match any coercion
rule. In Lambdapi, existential variables are unified after type checking is
done, and thus after coercions are applied.
In particular, implicit arguments won't be instanciated before applying
coercions.

For instance, let's define the following theory
```lp
constant symbol Set: TYPE;
symbol El: Set → TYPE;
constant symbol inhabited (_: Set): Set;
constant symbol inhabitant [a: Set] (_: El a): El (inhabited a);
```
Then define two encoded types `nat` and `int`
```lp
constant symbol nat: Set;
constant symbol z: El nat;
constant symbol int: Set;
constant symbol posint: El nat → El int;
coerce_rule coerce (El nat) (El int) $x ↪ posint $x;
```
Declare a function
```lp
symbol inhabitedInt: El (inhabited int) → TYPE;
```
Then typechecking `inhabitedInt z` yields the following
operations, where `=>` is type inference, `<=` is type checking
and `=` denotes unification constraints
```
inhabitedInt (inhabitant z) => ...
  inhabitant ?0 z => El (inhabited ?0)
	  inhabitant => Π a: Set, El (inhabited a)
		?0 <= Set
		z <= El ?0
		  z => El nat
			El nat == El ?0
	inhabitant ?0 z <= El (inhabited int)
	  El (inhabited ?0) = El (inhabited int)
```
Type checking fails because we obtain the constraints
`?0 = nat` and `?0 = int`.

The solution implemented in the fork is to call the unification algorithm
as soon as possible during type checking, that is,
[here](https://github.com/gabrielhdt/lambdapi/blob/e08034dea099262594c2493c7c4587ac9f396a1e/src/core/infer.ml#L65).

### Coercions may create unsafe symbols

Lambdapi reduces terms of the form `coerce A B t` to eliminate `coerce` from
terms, and replace the type checked term with the reduced term.
These reductions may generate unsafe symbols such as `pair'`.

For instance, in the Lambdapi encoding of Personoj, there is a coercion rule of
the form
```lp
coerce_rule coerce (El $a) (El (psub $a $p)) $x  ↪ pair $a $p $x _
```
But there is also a reduction from `pair a p e h` to `pair' a p e`,
so the reduction (with strategy weak head normal form) will reduce to `pair' a
p e`.

The fork introduces an advanced notion of *opacity*, the symbol `pair` is
declared and the reduction involved in coercions does not use rewrite rules of
opaque symbols.
