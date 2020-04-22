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

symbol plus_nat: η (ℕ ~> ℕ ~> ℕ)
set infix left 4 "+" ≔ plus_nat
symbol times_nat: η (ℕ ~> ℕ ~> ℕ)
set infix left 5 "*" ≔ times_nat
// Some properties of product
rule Z + $n ↪ $n
with $m + S $n ↪ S ($m + $n)
with $n + Z ↪ $n
with S $m + $n ↪ S ($m + $n)
rule Z * _ ↪ Z
with $m * (S $n) ↪ $m + ($m * $n)
with _ * Z ↪ Z
with (S $m) * $n ↪ $n + ($m * $n)

symbol nat_induction: ΠP: η ℕ → Bool,
                        ε (P 0) → (Πn, ε (P n) → ε (P (S n))) → Πn, ε (P n)

symbol ¬: Bool → Bool
rule ε (¬ $x) ↪ ε $x → Π(z: Bool), ε z
symbol eqnat: η (ℕ ~> ℕ ~> bool)
set infix left 3 "=ℕ" ≔ eqnat

symbol s_not_z: Πx: η ℕ, ε (¬ (Z =ℕ (S x)))

theorem times_comm: ε (∀ (λa, ∀ (λb, (a * b) =ℕ (b * a))))
proof
admit

definition {|nznat?|} ≔ λx, ¬ (Z =ℕ x)
definition ℕ* ≔ psub {|nznat?|}

definition Onz ≔ pair {ℕ} {{|nznat?|}} (S Z) (s_not_z Z) // One not zero

symbol Snz: η ℕ* → η ℕ* // Successor not zero
symbol nznat_induction:
  ΠP: η ℕ* → Bool, ε (P Onz) → (Πn, ε (P n) → ε (P (Snz n))) → Πn, ε (P n)

symbol div: η (ℕ ~> ℕ* ~> ℚ+)
set infix left 6 "/" ≔ div

symbol eqrat: η (ℚ+ ~> ℚ+ ~> bool)
set infix left 7 "=ℚ" ≔ eqrat
rule ($a / $b) =ℚ ($c / $d) ↪ ($a * (fst $d)) =ℕ ((fst $b) * $c)

definition imp P Q ≔ impd {P} (λ_, Q)
set infix left 6 "⊃" ≔ imp
definition false ≔ ∀ {bool} (λx, x)

theorem nzprod:
  ε (∀ {ℕ*}
       (λx, ∀ {ℕ*} (λy, {|nznat?|} ((fst x) * (fst y)))))
proof
  refine nznat_induction
           (λx, ∀ (λy: η ℕ*, (Z =ℕ (fst x * fst y)) ⊃ false)) ?xOnz ?xSnz
  // x = 1
  refine nznat_induction
           (λy, (Z =ℕ (fst Onz * fst y)) ⊃ false) ?yOnz ?ySnz
  simpl
  apply s_not_z Z
  // x = S n
  simpl
  assume n Hn
  refine snd (Snz n)
  assume n Hn
  refine nznat_induction
           (λz, (Z =ℕ (fst (Snz n) * (fst z))) ⊃ false) ?zOnz[n,Hn] ?zSnz[n,Hn]
  simpl
  refine snd (Snz n)
  assume m Hm
admit

symbol times_rat: η (ℚ+ ~> ℚ+ ~> ℚ+)
rule times_rat ($a / $b) ($c / $d) ↪
  let denom ≔ fst $b * (fst $d) in
  let prf ≔ nzprod $b $d in
  ($a * $c) / (pair denom prf)

theorem right_cancel:
  ε (∀ (λa, ∀ (λb, eqrat (times_rat (a / b) (fst b / Onz)) (a / Onz))))
proof
  assume x y
  simpl
  apply times_comm x (fst y)
qed
