// Close to PVS logic system: native equality,
// true, false, and if
require open personoj.encodings.lhol
             personoj.encodings.pvs_cert;

symbol false ≔ ∀ {prop} (λ x, x);
symbol true ≔ impd {false} (λ _, false);

symbol eq {s: Set}: El s → El s → El prop;
symbol = {s} ≔ @eq s;
set notation = infix 6;

symbol not p ≔ eq {prop} p false;
symbol ¬ ≔ not;
set notation ¬ prefix 5;

constant symbol if {s: Set} p: (Prf p → El s) → (Prf (¬ p) → El s) → El s;
rule Prf (if $p $then $else)
   ↪ (Π h: Prf $p, Prf ($then h)) → Π h: Prf (¬ $p), Prf ($else h);

symbol and p q ≔ if {prop} p q (λ _, false);
symbol or p q ≔ if {prop} p (λ _, true) q;
symbol imp p q ≔ if {prop} p q (λ _, true);
symbol ∧ ≔ and;
symbol ∨ ≔ or;
symbol ⊃ ≔ imp;
set notation ∧ infix left 4;
set notation ∨ infix left 3;
set notation ⊃ infix right 2;

set builtin "bot" ≔ false;
set builtin "top" ≔ true;
set builtin "not" ≔ not;
set builtin "imp" ≔ imp;
set builtin "and" ≔ and;
set builtin "or"  ≔ or;
