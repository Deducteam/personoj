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
3e5c7ee2388d233fdca05776009b5cb158e48d97,

``` sh
git clone github.com/Deducteam/lambdapi.git
git checkout 3e5c7ee2388d233fdca05776009b5cb158e48d97
make
make install
```

## Structure

- `encodings`: encoding of PVS into Dedukti
- `paper`: some specifications presented in papers
- `sandbox`: miscellaneous experiments
- `tools`: some additional scripts and utilities

## Dedukti coding conventions

- Dedukti types are capitalised  
  `Nat: TYPE`
- Predicates are suffixed with `_p`  
  `even_p: Nat → Prop`
- Documentation is commented with `///`
