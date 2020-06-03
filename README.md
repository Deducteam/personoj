# PVS proofs

Encodings of PVS and manual translations of PVS prelude and others.
Use with `lambdapi`.

The work consists essentially in translating fragments of the PVS standard
library, also known as _Prelude_. The prelude is available
- as raw PVS,
  <https://raw.githubusercontent.com/SRI-CSL/PVS/master/lib/prelude.pvs>;
- as a documentation in pdf,
  <http://pvs.csl.sri.com/doc/pvs-prelude.pdf>;
- as html, <http://www.cs.rug.nl/~grl/ar06/prelude.html>.


## Requirements

[`lambdapi`](https://github.com/Deducteam/lambdapi.git) later than 
fda8752584af52cdc8158a7a80bbe7fce5720616

## Structure

- `adlib`: additional libraries not in the prelude
- `encodings`: encoding of PVS into Dedukti
- `prelude`: parts of the PVS prelude
- `sandbox`: miscellaneous experiments
- `tools`: some additional scripts and utilities

## Dedukti coding conventions

- Dedukti types are capitalised  
  `Nat: TYPE`
- Predicates are suffixed with `_p`  
  `even_p: Nat → Bool`
- Documentation is commented with `///`
