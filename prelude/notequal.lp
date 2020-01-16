require open pvs_core prelude.booleans prelude.equalities

definition neq {T: Type} (x: eta T) (y: eta T) ≔ bnot (eq x y)
