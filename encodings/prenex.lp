// Prenex polymorphism
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

set declared "χ"
set declared "∇"
symbol Scheme: TYPE
symbol χ: Scheme ⇒ TYPE
symbol ∇: Set ⇒ Scheme
rule χ (∇ &X) → η &X

constant symbol forallS: (Set ⇒ Scheme) ⇒ Scheme
rule χ (forallS &p) → ∀T: Set, χ (&p T)

constant symbol forallB: (Set ⇒ Bool) ⇒ Bool
rule ε (forallB &P) → ∀x: Set, ε (&P x)
