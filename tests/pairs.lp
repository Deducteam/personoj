require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.deptype
require open personoj.encodings.prenex
require open personoj.encodings.pairs

symbol nat: Set
symbol plus: η ((Σ nat (λ_, nat)) ~> nat)

symbol z: η nat
symbol s: η (nat ~> nat)
set builtin "0" ≔ z
set builtin "+1" ≔ s

assert plus (pair_ss 3 4): η nat
