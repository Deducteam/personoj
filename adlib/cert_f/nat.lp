require open encodings.cert_f prelude.cert_f.booleans
prelude.cert_f.equalities prelude.cert_f.notequal
prelude.cert_f.naturalnumbers

definition not_zero ≔ neq 0
definition nznat ≔ psub nat not_zero

symbol times : Term nat ⇒ Term nat ⇒ Term nat

symbol prod_not_zero (x y: Term nat):
  Term (not_zero x) ⇒ Term (not_zero y) ⇒ Term (not_zero (times x y))

symbol prod_comm (x y: Term nat): Term (eq (times x y) (times y x))

// Constructor of nznat
definition nznatc (x: Term nat) (h: Term (not_zero x)) : Term nznat ≔
  pair not_zero x h

symbol one_not_zero: Term (not_zero 1)
