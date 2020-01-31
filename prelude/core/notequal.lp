require open encodings.pvs_core prelude.core.booleans prelude.core.equalities

definition neq {T: Type} (x: eta T) (y: eta T) ≔ bnot (eq x y)
