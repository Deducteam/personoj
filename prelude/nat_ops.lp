require open pvs_core prelude.notequal prelude.nat
symbol times : eta int ⇒ eta int ⇒ eta int
rule times &n 1 → &n

// x =/= 0 ∧ y =/= 0 ⇒ x * y =/= 0
symbol prod_not_zero (x y: eta int) :
  eps (neq x 0) ⇒ eps (neq y 0) ⇒ eps (neq (times x y) 0)
