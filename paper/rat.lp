require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

// Prelude with some logics operator
definition imp P Q ≔ impd {P} (λ_, Q)
set infix right 6 "⇒" ≔ imp
definition false ≔ ∀ {bool} (λx, x)
definition true ≔ false ⇒ false
symbol not: Bool → Bool
set prefix 8 "¬" ≔ not
rule Prf (¬ $x) ↪ Prf $x → Π(z: Bool), Prf z

// Nat top type
constant symbol nat: Set
// Presburger arithmetics
constant symbol s: El (nat ~> nat)
constant symbol z: El nat
constant symbol eqnat: El (nat ~> nat ~> bool)
symbol plus_nat: El (nat ~> nat ~> nat)
set infix left 4 "+" ≔ plus_nat
constant symbol s_not_z: Prf (∀ (λx, ¬ (eqnat z (s x))))
rule Prf (eqnat (s $n) (s $m)) ↪ Prf (eqnat $n $m) with Prf (eqnat z z) ↪ Prf true
rule $n + z ↪ $n with $n + s $m ↪ s ($n + $m)
symbol nat_ind:
  Prf (∀ {nat ~> bool} (λp, (p z) ⇒ (∀ (λn, p n ⇒ p (s n))) ⇒ (∀ (λn, p n))))

// System T
symbol rec_nat: El nat → El nat → (El nat → El nat → El nat) → El nat
rule rec_nat z $t0 _ ↪ $t0
rule rec_nat (s $u) $t0 $ts ↪ $ts $u (rec_nat $u $t0 $ts)

definition mult a b ≔ rec_nat a z (λ_ r, b + r)

symbol times_nat: El (nat ~> nat ~> nat)
set infix left 5 "*" ≔ times_nat
rule z * _ ↪ z
with $n * (s $m) ↪ $n + ($n * $m)
with _ * z ↪ z // (times_z_left)

// Declaration of a top type
constant symbol frac: Set

symbol eqfrac: El (frac ~> frac ~> bool)

theorem z_plus_n_n: Prf (∀ (λn, eqnat (z + n) n))
proof
  assume n
  refine nat_ind (λn, eqnat (z + n) n) _ _ n
  refine λx: Prf false, x
  assume n0 Hn
  apply Hn
qed

// The following theorem allows to remove rule (times_z_left)
// but doing so would require to have eqnat transitivity, which requires some
// more work. So it is left for now.
theorem n_times_z_z: Prf (∀ (λn, eqnat (z * n) z))
proof
  assume n
  refine nat_ind (λn, eqnat (z * n) z) _ _ n
  refine λx: Prf false, x
  assume n0 Hn
  refine Hn
qed

theorem times_comm: Prf (∀ (λa, ∀ (λb, eqnat (a * b) (b * a))))
proof
admit

definition nznat_p ≔ λn, ¬ (eqnat z n)
definition nznat ≔ psub nznat_p

theorem nzprod: Prf (∀ {nznat} (λx, ∀ {nznat} (λy, nznat_p (fst x * fst y))))
proof
admit

// Building rationals from natural numbers
symbol div: El (nat ~> nznat ~> frac)
set infix left 6 "/" ≔ div
rule eqfrac ($a / $b) ($c / $d) ↪ eqnat ($a * (fst $d)) ((fst $b) * $c)

// rule Prf (nat_p ($n / pair $n _)) ↪ Prf true
// Non linear rules break confluence

symbol times_frac: El (frac ~> frac ~> frac)
rule times_frac ($a / $b) ($c / $d)
   ↪ let denom ≔ fst $b * (fst $d) in
     let prf ≔ nzprod $b $d in
     ($a * $c) / (pair denom prf)


definition one_nz ≔ pair {nat} {nznat_p} (s z) (s_not_z z)

theorem right_cancel:
  Prf (∀ (λa, ∀ (λb, eqfrac (times_frac (a / b) (fst b / one_nz)) (a / one_nz))))
proof
  assume x y
  simpl
  apply times_comm x (fst y)
qed
