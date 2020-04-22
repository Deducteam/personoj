// Close to PVS logic system: native equality,
// true, false, and if
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

definition false ≔ ∀ {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

symbol eq {s: Set}: η s → η s → η bool
set infix left 6 "=" ≔ eq

definition not p ≔ eq {bool} p false
set prefix 5 "¬" ≔ not

constant symbol if {s: Set} p: (ε p → η s) → (ε (¬ p) → η s) → η s
rule ε (if $p $then $else)
   ↪ (Πh: ε $p, ε ($then h)) → Πh: ε (¬ $p), ε ($else h)

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
