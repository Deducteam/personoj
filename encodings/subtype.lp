require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

symbol subtype : Utype ⇒ Utype ⇒ Ubool
set infix left 6 "⊑" ≔ subtype

symbol Refl (a: Utype): ε (a ⊑ a)
symbol Trans (s t u: Utype): ε (s ⊑ t) ⇒ ε (t ⊑ u) ⇒ ε (s ⊑ u)

symbol Restr {a: Utype} (p: η a ⇒ Ubool): ε (psub p ⊑ a)
//symbol Subset
