require open encodings.cert_f
prelude.cert_f.equalities
prelude.cert_f.naturalnumbers
adlib.cert_f.nat

symbol mod2: Term nat ⇒ Term nat
rule mod2 (&n + 2) → mod2 &n
 and mod2 ( _ + 1) → 1
 and mod2       0  → 0

definition is_even (n: Term nat) ≔ eq (mod2 n) 0

definition Even ≔ psub nat is_even

definition evenify (n: Term nat) (h: Term (is_even n)) : Term Even ≔
  pair is_even n h

definition even_to_nat ≔ fst is_even

theorem twice_even_even (n: Term Even):
  Term (is_even (2 * even_to_nat n))
proof
assume n
simpl
qed
