require open encodings.pvs_core prelude.core.booleans prelude.core.equalities

definition exists {T: Type} (p : eta T ⇒ eta bool) ≔
    (bnot (all T (λ x, (bnot (p x)))))

set declared "∃"
definition ∃ {T: Type} ≔ @exists T

theorem exists_not (T: Type) (p: η T ⇒ η bool):
  ε (eq (∃ (λ x, ¬ (p x))) (¬ (all T p)))
proof
admit

theorem exists_or (T: Type) (p q: η T ⇒ η bool):
  ε (eq (∃ (λ x, (p x) ∨ (q x))) ((∃ p) ∨ (∃ q)))
proof admit
