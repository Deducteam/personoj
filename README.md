# Personoj -- Expressing PVS in Dedukti

*Personoj* allows to translate developments from the higher order, automated
proof assistant [PVS](http://pvs.csl.sri.com) into the universal proof checker
[Dedukti](https://deducteam.github.io). Personoj uses
[Lambdapi](https://github.com/Deducteam/lambdapi), an implementation of Dedukti
with meta-variables.

This repository contains 
- the encoding of PVS into the lambdaPi calculus modulo theory (written
  in lambdapi),
- patch for PVS that implement a translation from PVS theories to
  lambdapi,
- patches and programs to cross check PVS proofs in lambdapi

## Main requirements

Sub projects may require finer dependencies. We list here the main ones:

- [lambdapi](https://github.com/gabrielhdt/lambdapi.git), on the branch
  `refiner_why3quant`,
- SBCL version 1.4.16,
- [PVS sources](https://github.com/SRI-CSL/PVS.git), revision `pvs7.1`
- Allegro PVS

More documentation is available in the `docs` folder.
