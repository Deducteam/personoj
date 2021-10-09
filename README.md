# Personoj -- Expressing PVS specifications in Dedukti

*Personoj* allows to translate specifications from the higher order, automated
proof assistant [PVS](http://pvs.csl.sri.com) into the universal proof checker
[Dedukti](https://deducteam.github.io). Personoj uses
[Lambdapi](https://github.com/Deducteam/lambdapi), an implementation of Dedukti
with meta-variables.

This repository contains 
- lambdapi files that define the theory of PVS,
- lisp files that implement a translation from PVS to lambdapi,
- scripts and tools to translate PVS specifications.

## Requirements

- [lambdapi](https://github.com/gabrielhdt/lambdapi.git), revision `aae26f2d`
  from <https://github.com/gabrielhdt/lambdapi.git>
- [PVS sources](https://github.com/SRI-CSL/PVS.git), revision `pvs7.1`

[This repository](https://forge.tedomum.net/koizel/lambdapi-pvs-build) provides
files and tools for reproducible builds and translations.

## Installing the encoding

``` sh
bmake -C encoding install
```
will install the theory of PVS under the package name `personoj`.

## Exporting PVS to Dedukti

### Installation

Copy the content of `pvs-load.lisp` into `.pvs.lisp`, replacing `PVSDKPATH` with
the path (absolute or relative to `$HOME`) to the `exporter` directory.

### Usage

Assuming PVS sources have been downloaded to `$PVSPATH`, copy
`pvs-translation-tools` as `${PVSPATH}/translations`. Further instructions can
be found in `pvs-translation-tools/README.md`.
