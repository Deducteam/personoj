require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

definition false ≔ forall {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

symbol {|and|}: Bool ⇒ Bool ⇒ Bool
set infix left 6 "∧" ≔ {|and|}
constant symbol or: Bool ⇒ Bool ⇒ Bool
set infix left 5 "∨" ≔ or

set builtin "bot" ≔ false
set builtin "top" ≔ true

definition not P ≔ impd {P} (λ_, false)
set prefix 7 "¬" ≔ not
