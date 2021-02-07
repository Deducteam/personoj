// Definition based on implication
require open personoj.encodings.lhol;

symbol false ≔ ∀ {prop} (λ x, x);
symbol true ≔ impd {false} (λ _, false);

symbol not P ≔ impd {P} (λ _, false);
set prefix 5 "¬" ≔ not;

symbol imp P Q ≔ impd {P} Q;
set infix 2 "⊃" ≔ imp;

symbol and P Q ≔ ¬ (P ⊃ (λ h, ¬ (Q h)));
set infix 4 "∧" ≔ and;
symbol or P Q ≔ (¬ P) ⊃ Q;
set infix 3 "∨" ≔ or;

set builtin "bot" ≔ false;
set builtin "top" ≔ true;
set builtin "not" ≔ not;
set builtin "imp" ≔ imp;
set builtin "and" ≔ and;
set builtin "or"  ≔ or;

symbol if {s: Set} (p: Prop): (Prf p → El s) → (Prf (¬ p) → El s) → El s;
rule if {prop} $p $then $else ↪ ($p ⊃ $then) ⊃ (λ _, (¬ $p) ⊃ $else);

symbol iff P Q ≔ (P ⊃ (λ _, Q)) ∧ ((λ _, Q ⊃ (λ _, P)));
