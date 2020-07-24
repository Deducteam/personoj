// Close to PVS logic system: native equality,
// true, false, and if
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

definition false ≔ ∀ {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

symbol eq {s: Set}: El s → El s → El bool
set infix left 6 "=" ≔ eq

definition not p ≔ eq {bool} p false
set prefix 5 "¬" ≔ not

constant symbol if {s: Set} p: (Prf p → El s) → (Prf (¬ p) → El s) → El s
rule Prf (if $p $then $else)
   ↪ (Πh: Prf $p, Prf ($then h)) → Πh: Prf (¬ $p), Prf ($else h)

definition and p q ≔ if {bool} p q (λ_, false)
definition or p q ≔ if {bool} p (λ_, true) q
definition imp p q ≔ if {bool} p q (λ_, true)
set infix left 4 "∧" ≔ and
set infix left 3 "∨" ≔ or
set infix left 2 "⊃" ≔ imp

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "not" ≔ not
set builtin "imp" ≔ imp
set builtin "and" ≔ and
set builtin "or"  ≔ or
