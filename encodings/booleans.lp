require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

definition false ≔ forall {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

definition not P ≔ impd {P} (λ_, false)
set prefix 7 "¬" ≔ not

set builtin "bot" ≔ false
set builtin "top" ≔ true

