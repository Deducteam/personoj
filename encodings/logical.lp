// Definition based on implication
require open personoj.encodings.lhol;

symbol false ≔ ∀ {prop} (λ x, x);
symbol true ≔ impd {false} (λ _, false);

symbol not P ≔ impd {P} (λ _, false);
symbol ¬ ≔ not;
set notation ¬ prefix 5;

symbol imp P Q ≔ impd {P} Q;
symbol ⊃ ≔ imp;
set notation ⊃ infix right 2;

symbol and P Q ≔ ¬ (P ⊃ (λ h, ¬ (Q h)));
symbol or P Q ≔ (¬ P) ⊃ Q;
symbol ∧ ≔ and; symbol ∨ ≔ or;
set notation ∧ infix 4;
set notation ∨ infix 3;

set builtin "bot" ≔ false;
set builtin "top" ≔ true;
set builtin "not" ≔ not;
set builtin "imp" ≔ imp;
set builtin "and" ≔ and;
set builtin "or"  ≔ or;

symbol if {s: Set} (p: Prop): (Prf p → El s) → (Prf (¬ p) → El s) → El s;
rule if {prop} $p $then $else ↪ ($p ⊃ $then) ⊃ (λ _, (¬ $p) ⊃ $else);

symbol iff P Q ≔ (P ⊃ (λ _, Q)) ∧ ((λ _, Q ⊃ (λ _, P)));
