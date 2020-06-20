require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

// Prelude with some logics operator
definition imp P Q ≔ impd {P} (λ_, Q)
set infix right 6 "⇒" ≔ imp
definition false ≔ ∀ {bool} (λx, x)
definition true ≔ false ⇒ false
symbol not: Bool → Bool
set prefix 8 "¬" ≔ not
rule ε (¬ $x) ↪ ε $x → Π(z: Bool), ε z

// Declaration of a top type
constant symbol rat: Set

symbol eqrat: η (rat ~> rat ~> bool)

// Definition of a sub-type of ‘rat’
symbol nat_p: η (rat ~> bool) // Recogniser
definition nat ≔ psub nat_p

// Presburger arithmetics
constant symbol s: η (nat ~> nat)
constant symbol z: η nat
constant symbol eqnat: η (nat ~> nat ~> bool)
symbol plus_nat: η (nat ~> nat ~> nat)
set infix left 4 "+" ≔ plus_nat
constant symbol s_not_z: ε (∀ (λx, ¬ (eqnat z (s x))))
rule ε (eqnat (s $n) (s $m)) ↪ ε (eqnat $n $m) with ε (eqnat z z) ↪ ε true
rule $n + z ↪ $n with $n + s $m ↪ s ($n + $m)
symbol nat_ind:
  ε (∀ {nat ~> bool} (λp, (p z) ⇒ (∀ (λn, p n ⇒ p (s n))) ⇒ (∀ (λn, p n))))

theorem z_plus_n_n: ε (∀ (λn, eqnat (z + n) n))
proof
  assume n
  refine nat_ind (λn, eqnat (z + n) n) _ _ n
  refine λx: ε false, x
  assume n0 Hn
  apply Hn
qed

symbol times_nat: η (nat ~> nat ~> nat)
set infix left 5 "*" ≔ times_nat
rule z * _ ↪ z
with $n * (s $m) ↪ $n + ($n * $m)
with _ * z ↪ z // (times_z_left)

// The following theorem allows to remove rule (times_z_left)
// but doing so would require to have eqnat transitivity, which requires some
// more work. So it is left for now.
theorem n_times_z_z: ε (∀ (λn, eqnat (z * n) z))
proof
  assume n
  refine nat_ind (λn, eqnat (z * n) z) _ _ n
  refine λx: ε false, x
  assume n0 Hn
  refine Hn
qed

theorem times_comm: ε (∀ (λa, ∀ (λb, eqnat (a * b) (b * a))))
proof
admit

definition nznat_p ≔ λn, ¬ (eqnat z n)
definition nznat ≔ psub nznat_p

theorem nzprod: ε (∀ {nznat} (λx, ∀ {nznat} (λy, nznat_p (fst x * fst y))))
proof
admit

// Building rationals from natural numbers
symbol frac: η (nat ~> nznat ~> rat)
set infix left 6 "/" ≔ frac
rule eqrat ($a / $b) ($c / $d) ↪ eqnat ($a * (fst $d)) ((fst $b) * $c)

// rule ε (nat_p ($n / pair $n _)) ↪ ε true
// Non linear rules break confluence

symbol times_rat: η (rat ~> rat ~> rat)
rule times_rat ($a / $b) ($c / $d)
   ↪ let denom ≔ fst $b * (fst $d) in
     let prf ≔ nzprod $b $d in
     ($a * $c) / (pair denom prf)


definition one_nz ≔ pair {nat} {nznat_p} (s z) (s_not_z z)

theorem right_cancel:
  ε (∀ (λa, ∀ (λb, eqrat (times_rat (a / b) (fst b / one_nz)) (a / one_nz))))
proof
  assume x y
  simpl
  apply times_comm x (fst y)
qed
