require open encodings.cert_f prelude.cert_f.booleans
prelude.cert_f.equalities prelude.cert_f.notequal
prelude.cert_f.naturalnumbers

symbol times : Term nat ⇒ Term nat ⇒ Term nat
symbol plus : Term nat ⇒ Term nat ⇒ Term nat
set infix left 6 "+" ≔ plus
set infix left 7 "*" ≔ times

rule (succ &n) +       &m  → succ (&n + &m)
 and        0  +       &m  → &m
 and       &n  + (succ &m) → succ (&n + &m)
 and       &n  +        0  → &n

rule (succ &n) *       &m  → &n * &m + &m
 and        0  *        _  → 0
 and       &n  * (succ &m) → &n * &m + &n
 and        _  * 0         → 0

symbol prod_comm (x y: Term nat): Term (eq (times x y) (times y x))


//
// Non zero naturals
//

definition not_zero ≔ neq 0
type not_zero
symbol prod_not_zero (x y: Term nat):
  Term (not_zero x) ⇒ Term (not_zero y) ⇒ Term (not_zero (times x y))

definition nznat ≔ psub nat not_zero

// Constructor of nznat
definition nznatc (x: Term nat) (h: Term (not_zero x)) : Term nznat ≔
  pair not_zero x h

symbol one_not_zero: Term (not_zero 1)
