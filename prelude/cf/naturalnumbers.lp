require open encodings.cf prelude.cf.booleans

constant symbol nat : @Term Kind uType
constant symbol zero : @Term Type nat
symbol succ : @Term Type nat ⇒ @Term Type nat

set builtin "0" ≔ zero
set builtin "+1" ≔ succ
