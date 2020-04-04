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
- `adlib` contains additional libraries not in the prelude
- `encodings` contains encodings of PVS into Dedukti, most of the work is done
  using `cert_f` encoding
- `prelude` contains parts of the PVS prelude
- `sandbox` contains miscellaneous experiments
- `tools` contains some additional scripts and utilities
- `alternatives` contains files in other encodings
