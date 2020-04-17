require open encodings.pvs_core prelude.core.booleans prelude.core.equalities
prelude.core.notequal adlib.core.nat
require adlib.core.nat_ops as N

constant symbol rat : Type
constant symbol zero : η rat

symbol frac (_ m : η int) (pi : ε (neq m 0)) : η rat

symbol div : η rat → η rat → η rat
symbol times : η rat → η rat → η rat
set infix 5 "*" ≔ times
set infix 6 "/" ≔ div

rule times (frac $A $B $X) (frac $C $D $Y) ↪
  frac (N.times $A $C) (N.times $B $D) (N.prod_not_zero $B $D $X $Y)

symbol rateq : η rat → η rat → η Prop
rule rateq (frac $A $B _) (frac $C $D _) ↪
  eq (N.times $A $D) (N.times $B $C)

// (a/b) * (b/1) = (a/1)
theorem right_cancellation (a b : η int) (pi : ε (neq b 0)) (pi' : ε (neq 1 0)):
  ε (rateq ((frac a b pi) * (frac b 1 pi')) (frac a 1 pi'))
proof
assume a b pi pi'
simpl
refine N.prod_comm a b
qed
