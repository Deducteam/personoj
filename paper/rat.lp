require open
personoj.encodings.lhol
personoj.encodings.pvs_cert

set declared "ℕ"
set declared "ℚ+"
set declared "ℕ*"
set declared "¬"

constant symbol ℕ: Set
constant symbol Z: η ℕ
constant symbol S: η (ℕ ~> ℕ)
constant symbol ℚ+: Set

set builtin "0" ≔ Z
set builtin "+1" ≔ S

symbol times_nat: η (ℕ ~> ℕ ~> ℕ)
set infix left 5 "*" ≔ times_nat
// Some properties of product
rule Z * _ → Z
 and _ * Z → Z
 and (S Z) * &n → &n
 and &n * (S Z) → &n

symbol nat_induction
     : ∀P: η ℕ ⇒ Bool, ε (P 0) ⇒ (∀n, ε (P n) ⇒ ε (P (S n))) ⇒ ∀n, ε (P n)

symbol ¬: Bool ⇒ Bool
rule ε (¬ &x) → ε &x ⇒ ∀(z: Bool), ε z
symbol eqnat: η (ℕ ~> ℕ ~> bool)
set infix left 7 "=ℕ" ≔ eqnat

theorem times_comm: ε (forall (λa, forall (λb, (a * b) =ℕ (b * a))))
proof
admit

definition {|nznat?|} ≔ λx, ¬ (Z =ℕ x)
definition ℕ* ≔ psub {|nznat?|}

symbol one_not_z: ε (¬ (eqnat Z (S Z)))
definition Onz ≔ pair {ℕ} {{|nznat?|}} (S Z) (one_not_z) // One not zero

symbol Snz : η ℕ* ⇒ η ℕ* // Successor not zero
symbol nznat_induction
     : ∀P: η ℕ* ⇒ Bool, ε (P Onz) ⇒ (∀n, ε (P n) ⇒ ε (P (Snz n))) ⇒ ∀n, ε (P n)

symbol div: η (ℕ ~> ℕ* ~> ℚ+)
set infix left 6 "/" ≔ div

symbol eqrat: η (ℚ+ ~> ℚ+ ~> bool)
set infix left 7 "=ℚ" ≔ eqrat
rule (&a / &b) =ℚ (&c / &d) → (&a * (fst &d)) =ℕ ((fst &b) * &c)

theorem nzprod
: ε (forall
     {ℕ*}
     (λx, forall {ℕ*} (λy, {|nznat?|} ((fst x) * (fst y)))))
proof
  assume x
admit

definition nat_of_nznat ≔ fst {ℕ} {{|nznat?|}}

symbol times_rat: η (ℚ+ ~> ℚ+ ~> ℚ+)
rule times_rat (&a / &b) (&c / &d) →
     let denom ≔ fst &b * (fst &d) in
     let prf ≔ nzprod &b &d in
     (&a * &c) / (pair denom prf)

theorem right_cancel
: ε (forall
     (λa, forall
          (λb, eqrat (times_rat (a / b) (fst b / Onz)) (a / Onz))))
proof
  assume x y
  simpl
  apply times_comm x (fst y)
qed
