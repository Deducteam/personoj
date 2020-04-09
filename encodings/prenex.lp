// Prenex polymorphism
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

set declared "χ"
set declared "∇"
symbol Scheme: TYPE
symbol χ: Scheme ⇒ TYPE
symbol ∇: Set ⇒ Scheme
rule χ (∇ &X) → η &X

symbol forallp_scheme: (Set ⇒ Scheme) ⇒ Scheme
rule χ (forallp_scheme &p) → ∀T: Set, χ (&p T)

symbol forallp_bool: (Set ⇒ Bool) ⇒ Bool
rule ε (forallp_bool &P) → ∀x: Set, ε (&P x)
