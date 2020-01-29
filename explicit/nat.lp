require open pvs_cert explicit.pts_bindings

constant symbol int : Term Kind uType
symbol zero : Term Type int
injective symbol succ : Term Type int ⇒ Term Type int

set builtin "0" ≔ zero
set builtin "+1" ≔ succ
