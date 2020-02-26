require open encodings.cert_f prelude.cert_f.booleans prelude.cert_f.equalities

definition neq {T: Term uType} (x y: Term T) ≔ bnot (eq x y)
set infix left 6 "/=" ≔ neq
