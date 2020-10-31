// Definition based on implication
require open personoj.encodings.lhol

definition false ≔ ∀ {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

definition not P ≔ impd {P} (λ_, false)
set prefix 5 "¬" ≔ not

definition imp P Q ≔ impd {P} Q
set infix 2 "⊃" ≔ imp

definition and P Q ≔ ¬ (P ⊃ (λh, ¬ (Q h)))
set infix 4 "∧" ≔ and
definition or P Q ≔ (¬ P) ⊃ Q
set infix 3 "∨" ≔ or

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "not" ≔ not
set builtin "imp" ≔ imp
set builtin "and" ≔ and
set builtin "or"  ≔ or

symbol if {s: Set} p: (Prf p → El s) → (Prf (¬ p) → El s) → El s
rule if {bool} $p $then $else ↪ ($p ⊃ $then) ⊃ (λ_, (¬ $p) ⊃ $else)

definition iff P Q ≔ (P ⊃ (λ_, Q)) ∧ ((λ_, Q ⊃ (λ_, P)))

symbol eq {s: Set}: El s → El s → El bool
set infix 2 "=" ≔ eq
set builtin "eq" ≔ eq
definition neq {s: Set} (x y: El s) ≔ ¬ (x = y)
set infix 2 "≠" ≔ neq

// Leibniz equality
rule Prf ($x = $y) ↪ Πp: El (_ ~> bool), Prf (p $x) → Prf (p $y)

// Some theorems for equality
theorem eq_refl {a: Set} (x: El a): Prf (x = x)
proof
  assume a x p h
  apply h
qed
set builtin "refl" ≔ eq_refl

theorem eq_trans {a: Set} (x y z: El a) (_: Prf (x = y)) (_: Prf (y = z))
      : Prf (x = z)
proof
  assume a x y z hxy hyz p px
  refine hyz p (hxy p px)
qed
