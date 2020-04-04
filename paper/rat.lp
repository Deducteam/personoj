require open
personoj.encodings.lhol
personoj.encodings.pvs_cert

set declared "ℕ"
set declared "ℚ+"
set declared "ℕ*"
set declared "¬"

constant symbol ℕ: Utype
constant symbol Z: η ℕ
injective symbol S: η (ℕ ~> ℕ)
constant symbol ℚ+: Utype

symbol times_nat: η (ℕ ~> ℕ ~> ℕ)
set infix left 5 "*" ≔ times_nat
// Some properties of product
rule Z * _ → Z
 and _ * Z → Z
 and (S Z) * &n → &n
 and &n * (S Z) → &n

symbol ¬: Ubool ⇒ Ubool
rule ε (¬ &x) → ε &x ⇒ ∀(z: Ubool), ε z
symbol eqnat: η (ℕ ~> ℕ ~> cbool)
set infix left 7 "=ℕ" ≔ eqnat

theorem times_comm: ε (forall (λa, forall (λb, (a * b) =ℕ (b * a))))
proof
admit

definition {|nznat?|} ≔ λx, ¬ (Z =ℕ x)
definition ℕ* ≔ psub {|nznat?|}

symbol div: η (ℕ ~> ℕ* ~> ℚ+)
set infix left 6 "/" ≔ div

symbol eqrat: η (ℚ+ ~> ℚ+ ~> cbool)
set infix left 7 "=ℚ" ≔ eqrat
rule (&a / &b) =ℚ (&c / &d) → (&a * (fst &d)) =ℕ ((fst &b) * &c)

theorem nzprod
: ε (forall
     {ℕ*}
     (λx, forall {ℕ*} (λy, {|nznat?|} ((fst x) * (fst y)))))
proof
admit

definition nat_of_nznat ≔ fst {ℕ} {{|nznat?|}}

symbol times_rat: η (ℚ+ ~> ℚ+ ~> ℚ+)
rule times_rat (&a / &b) (&c / &d) →
     let denom ≔ fst &b * (fst &d) in
     let prf ≔ nzprod &b &d in
     (&a * &c) / (pair denom prf)

symbol one_not_z: ε (¬ (eqnat Z (S Z)))
definition nz_one ≔ pair {ℕ} {{|nznat?|}} (S Z) (one_not_z)

theorem right_cancel
: ε (forall
     (λa, forall
          (λb, eqrat (times_rat (a / b) (fst b / nz_one)) (a / nz_one))))
proof
  assume x y
  simpl
  apply times_comm x (fst y)
qed
