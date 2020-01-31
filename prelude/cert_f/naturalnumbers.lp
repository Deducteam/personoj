require open encodings.cert_f prelude.cert_f.booleans

constant symbol nat : Term uType
constant symbol zero : Term nat
injective symbol succ : Term nat ⇒ Term nat

set builtin "0" ≔ zero
set builtin "+1" ≔ succ
