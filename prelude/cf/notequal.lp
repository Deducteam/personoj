require open encodings.cf prelude.cf.booleans prelude.cf.equalities

definition neq {T: Term uType} (x y: Term T) ≔ bnot (eq x y)
