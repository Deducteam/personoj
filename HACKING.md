# Personoj developer guide


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
