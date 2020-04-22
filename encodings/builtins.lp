require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

constant symbol {|!Number!|}: θ {|set|}

constant symbol zero: η {|!Number!|}
constant symbol succ: η ({|!Number!|} ~> {|!Number!|})

set builtin "0" ≔ zero
set builtin "+1" ≔ succ

// In PVS, the axipms 1 ≠ 2, 1≠3, ... are built in
// Here we use the decidable equality
rule ε (succ $n = succ $m) ↪ ε ($n = $m)
with ε (zero = succ _) ↪ ε false
with ε (zero = zero) ↪ ε true
