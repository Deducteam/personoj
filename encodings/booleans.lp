require open personoj.encodings.lhol
  personoj.encodings.pvs_cert

definition false ≔ forall {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

set builtin "bot" ≔ false
set builtin "top" ≔ true
