require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.deptype
require open personoj.encodings.prenex
require open personoj.encodings.pairs

symbol nat: Set
symbol plus: El ((Σ nat (λ_, nat)) ~> nat)

symbol z: El nat
symbol s: El (nat ~> nat)
set builtin "0" ≔ z
set builtin "+1" ≔ s

assert plus (pair_ss 3 4): El nat
