// Definition based on implication
require open psnj.encoding.lhol;

symbol false ≔ ∀ [prop] (λ x: El prop, x);
symbol true ≔ false ⇒ (λ _: Prf false, false);
symbol ¬ P ≔ P ⇒ (λ _, false);         notation ¬ prefix 5;
symbol ∧ P Q ≔ ¬ (P ⇒ (λ h, ¬ (Q h))); notation ∧ infix 4;
symbol ∨ P Q ≔ (¬ P) ⇒ Q;              notation ∨ infix 3;

symbol if [s: Set] (p: Prop): (Prf p → El s) → (Prf (¬ p) → El s) → El s;
rule if [prop] $p $then $else ↪ ($p ⇒ $then) ⇒ (λ _, (¬ $p) ⇒ $else);

symbol iff P Q ≔ (P ⇒ (λ _, Q)) ∧ (λ _, Q ⇒ (λ _, P));

symbol ∃ [T: Set] (P: El T → El prop) ≔ ¬ (∀ (λ x, ¬ (P x)));
