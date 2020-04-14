// Boolean operators defined from if.
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans
  personoj.encodings.if

// Definition of logical connectors with if3
definition band p q ≔ if {bool} p q (λ_, false)
definition bor p q ≔ if {bool} p (λ_, true) q
definition imp p q ≔ if {bool} p q (λ_, true)
set infix left 7 "∧" ≔ band
set infix left 6 "∧" ≔ bor
set infix left 5 "⊃" ≔ imp
