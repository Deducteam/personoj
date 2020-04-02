require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

symbol subtype : Utype ⇒ Utype ⇒ Ubool
set infix left 6 "⊑" ≔ subtype

symbol Refl (a: Utype): ε (a ⊑ a)
symbol Trans (s t u: Utype): ε (s ⊑ t) ⇒ ε (t ⊑ u) ⇒ ε (s ⊑ u)

symbol Restr {a: Utype} (p: η a ⇒ Ubool): ε (psub p ⊑ a)

symbol Arr (t u1 u2: Utype): ε (u1 ⊑ u2) ⇒ ε ((t ~> u1) ⊑ (t ~> u2))
symbol Darr (d: Utype) (r1: η d ⇒ Utype) (r2: η d ⇒ Utype)
: ε (forall (λx, (r1 x) ⊑ (r2 x))) ⇒ ε ((arrd r1) ⊑ (arrd r2))
//symbol Subset
