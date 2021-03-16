// Definition based on implication
require open personoj.encodings.lhol;

symbol false ≔ ∀ {prop} (λ x, x);
symbol true ≔ impd {false} (λ _, false);

symbol not P ≔ impd {P} (λ _, false);
symbol ¬ ≔ not;
notation ¬ prefix 5;

symbol imp P Q ≔ impd {P} Q;
symbol ⊃ ≔ imp;
notation ⊃ infix right 2;

symbol and P Q ≔ ¬ (P ⊃ (λ h, ¬ (Q h)));
symbol or P Q ≔ (¬ P) ⊃ Q;
symbol ∧ ≔ and; symbol ∨ ≔ or;
notation ∧ infix 4;
notation ∨ infix 3;

builtin "bot" ≔ false;
builtin "top" ≔ true;
builtin "not" ≔ not;
builtin "imp" ≔ imp;
builtin "and" ≔ and;
builtin "or"  ≔ or;

symbol if {s: Set} (p: Prop): (Prf p → El s) → (Prf (¬ p) → El s) → El s;
rule if {prop} $p $then $else ↪ ($p ⊃ $then) ⊃ (λ _, (¬ $p) ⊃ $else);

symbol iff P Q ≔ (P ⊃ (λ _, Q)) ∧ ((λ _, Q ⊃ (λ _, P)));
