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

[`lambdapi`](https://github.com/Deducteam/lambdapi.git), but a development
version,

``` sh
git clone github.com/gabrielhdt/lambdapi.git
git checkout 3ac05539d019206ab191c21c8b487e9fb0750aee
make
make install
```

## Structure

- `personoj`: encoding of PVS into Dedukti
- `personoj/extra`: encodings that are not needed for PVS
- `personoj/examples`: examples of PVS development in Lambdapi
- `sandbox`: miscellaneous experiments

## Dedukti coding conventions

- Dedukti types are capitalised  
  `Nat: TYPE`
- Predicates are suffixed with `_p`  
  `even_p: Nat → Prop`
- Documentation is commented with `///`
