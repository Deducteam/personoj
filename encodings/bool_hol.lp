// Definition based on implication
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

definition false ≔ forall {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

definition not P ≔ impd {P} (λ_, false)
set prefix 5 "¬" ≔ not

definition imp P Q ≔ impd {P} Q
set infix 2 "⊃" ≔ imp

definition {|and|} P Q ≔ ¬ (P ⊃ (λh, ¬ (Q h)))
set infix 4 "∧" ≔ {|and|}
definition or P Q ≔ (¬ P) ⊃ Q
set infix 3 "∨" ≔ or

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "not" ≔ not
set builtin "imp" ≔ imp
set builtin "and" ≔ {|and|}
set builtin "or"  ≔ or

symbol if {s: Set} p: (ε p ⇒ η s) ⇒ (ε (¬ p) ⇒ η s) ⇒ η s
rule if {bool} &p &then &else → (&p ⊃ &then) ⊃ (λ_, ¬ &p ⊃ &else)
