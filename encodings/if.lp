// Close to PVS logic system: native equality,
// true, false, and if
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

symbol false ≔ ∀ {prop} (λx, x)
symbol true ≔ impd {false} (λ_, false)

symbol eq {s: Set}: El s → El s → El prop
set infix left 6 "=" ≔ eq

symbol not p ≔ eq {prop} p false
set prefix 5 "¬" ≔ not

constant symbol if {s: Set} p: (Prf p → El s) → (Prf (¬ p) → El s) → El s
rule Prf (if $p $then $else)
   ↪ (Πh: Prf $p, Prf ($then h)) → Πh: Prf (¬ $p), Prf ($else h)

symbol and p q ≔ if {prop} p q (λ_, false)
symbol or p q ≔ if {prop} p (λ_, true) q
symbol imp p q ≔ if {prop} p q (λ_, true)
set infix left 4 "∧" ≔ and
set infix left 3 "∨" ≔ or
set infix left 2 "⊃" ≔ imp

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "not" ≔ not
set builtin "imp" ≔ imp
set builtin "and" ≔ and
set builtin "or"  ≔ or
