require open pvs_core prelude.notequal prelude.nat prelude.equalities
symbol times : eta int ⇒ eta int ⇒ eta int
rule times &n 1  → &n

set infix left 6 "*" ≔ times

// x =/= 0 ∧ y =/= 0 ⇒ x * y =/= 0
symbol prod_not_zero (x y: eta int) :
  eps (neq x 0) ⇒ eps (neq y 0) ⇒ eps (neq (times x y) 0)

symbol prod_comm (x y : eta int) : eps (eq (x * y) (y * x))
