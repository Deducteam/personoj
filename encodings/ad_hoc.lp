// Prenex ad-hoc polymorphism
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.equality
  personoj.encodings.prenex
  personoj.encodings.subtype

symbol forallah_scheme: Set ⇒ (Set ⇒ Scheme) ⇒ Scheme
rule χ (forallah_scheme &T &C) → ∀U: Set, ε (U ⊑ &T) ⇒ χ (&C U)

symbol forallah_bool: Set ⇒ (Set ⇒ Bool) ⇒ Bool
rule ε (forallah_bool &T &C) → ∀U: Set, ε (U ⊑ &T) ⇒ ε (&C U)
