require open pvs_core booleans equalities

definition neq (T: Type) (x: eta T) (y: eta T) ≔ bnot (eq T x y)
