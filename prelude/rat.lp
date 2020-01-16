require open pvs_core prelude.booleans prelude.equalities prelude.notequal
prelude.nat
require prelude.nat_ops as N

constant symbol rat : Type
constant symbol zero : eta rat

symbol frac (n : eta int) (m : eta int) (pi : eps (neq m 0)) : eta rat

symbol div : eta rat ⇒ eta rat ⇒ eta rat
symbol times : eta rat ⇒ eta rat ⇒ eta rat
set infix 5 "*" ≔ times
set infix 6 "/" ≔ div

rule times (frac &A &B &X) (frac &C &D &Y) →
  frac (N.times &A &C) (N.times &B &D) (N.prod_not_zero &B &D &X &Y)

symbol rateq : eta rat ⇒ eta rat ⇒ eta Prop
rule rateq (frac &A &B &Pi) (frac &C &D &Pi') →
  eq (N.times &A &D) (N.times &B &C)

// (a/b) * (b/1) = (a/1)
theorem right_cancellation (a b : eta int) (pi : eps (neq b 0)) (pi' : eps (neq 1 0)):
  eps (rateq ((frac a b pi) * (frac b 1 pi')) (frac a 1 pi'))
proof
assume a b pi pi'
simpl
refine N.prod_comm a b
qed
