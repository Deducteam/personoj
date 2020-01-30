require open encodings.cf prelude.cf.booleans prelude.cf.equalities

definition neq {T: @Term Kind uType} (x y: @Term Type T) ≔ bnot (@eq T x y)
