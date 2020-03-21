require open encodings.pvs_core prelude.core.notequal
// Not part of prelude as integers are built in pvs

constant symbol int : Type
symbol zero : eta int
symbol succ (_: eta int) : eta int

set builtin "0" ≔ zero
set builtin "+1" ≔ succ
