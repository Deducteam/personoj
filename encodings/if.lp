require open personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans

// Polymorphic if
symbol if {s: Set} (p: Bool): (ε p ⇒ η s) ⇒ (ε (¬ p) ⇒ η s) ⇒ η s
rule if true &then _ → &then (λx, x)
 and if false _ &else → &else (λx, x)

// Definition of logical connectors with if3
definition band p q ≔ if {bool} p (λ_, q) (λ_, false)
definition bor p q ≔ if {bool} p (λ_, true) (λ_, q)
definition imp p q ≔ if {bool} p (λ_, q) (λ_, true)
set infix left 7 "∧" ≔ band
set infix left 6 "∧" ≔ bor
set infix left 5 "⊃" ≔ imp
