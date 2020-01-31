require open encodings.pvs_core prelude.core.booleans prelude.core.equalities

definition exists (T: Type) (p : eta T ⇒ eta bool) ≔
    (bnot (all T (λ x, (bnot (p x)))))

theorem exists_not :
  ∀ T : Type,
  ∀ p : eta T ⇒ eta bool,
  eps (eq bool (exists T (λ x, ¬ (p x))) (¬ (all T p)))
proof admit

theorem exists_or (T: Type) (p q: eta T ⇒ eta bool):
  eps (eq bool
        (exists T (λ x, (p x) ∨ (q x)))
        ((exists T p) ∨ (exists T q)))
  proof admit
