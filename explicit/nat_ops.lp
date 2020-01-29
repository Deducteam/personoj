require open pvs_cert explicit.pts_bindings explicit.nat explicit.notequal

symbol times : Term Type int ⇒ Term Type int ⇒ Term Type int
rule times &n 1 → &n

set infix left 6 "*" ≔ times

type λ x : Term Type int, neq int x 0

symbol prod_not_zero (x y : Term Type int) :
  Term Prop (neq int x 0) ⇒ Term Prop (neq int y 0) ⇒
  Term Prop (neq int (x * y) 0)
