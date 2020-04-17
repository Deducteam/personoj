require open pvs_core prelude.booleans prelude.equalities prelude.notequal
require open prelude.nat
require prelude.nat_ops as N

constant symbol rat : Type
constant symbol zrat : η rat

definition intnz ≔ psub int (λ x, neq 0 x)

symbol frac : η int → η intnz → η rat

symbol times : η rat → η rat → η rat
set infix 5 "*" ≔ times
rule times (frac $a $b) (frac $c $d) ↪ frac (N.times $a $c) (N.times $b $d)

symbol rateq : η rat → η rat → η bool
rule rateq (frac $a $b) (frac $c $d) ↪ eq (N.times $a $d) (N.times $b $c)

theorem right_cancellation (a : η int) (b : η intnz) :
  ε (rateq ((frac a b) * (frac b 1)) (frac a 1))
  proof
  admit
