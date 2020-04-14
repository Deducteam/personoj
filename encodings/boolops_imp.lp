// Boolean operators defined from dependent implication.
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans

definition imp P Q ≔ impd {P} (λ_, Q)
set infix 4 "⊃" ≔ imp

definition band P Q ≔ ¬ (P ⊃ (¬ Q))
definition bor P Q ≔ (¬ P) ⊃ Q
set infix 6 "∧" ≔ band
set infix 5 "∨" ≔ bor

definition biff P Q ≔ (P ⊃ Q) ∧ (Q ⊃ P)
set infix 7 "⇔" ≔ biff

definition when P Q ≔ Q ⊃ P

set builtin "imp" ≔ imp
set builtin "and" ≔ band
set builtin "or"  ≔ bor
