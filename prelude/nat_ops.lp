require open pvs_core prelude.notequal prelude.nat prelude.equalities

symbol times : eta int ⇒ eta int ⇒ eta int
rule times &n 1  → &n

set infix left 6 "*" ≔ times

// x =/= 0 ∧ y =/= 0 ⇒ x * y =/= 0
symbol prod_not_zero (x y: η int) :
  ε (neq x 0) ⇒ ε (neq y 0) ⇒ ε (neq (x * y) 0)

symbol prod_comm (x y : η int) : ε (eq (x * y) (y * x))
