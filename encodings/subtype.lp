require open personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans

set declared "↑"
set declared "μ"
set declared "μ₀"
set declared "π"

symbol S_Refl (a: Set): ε (a ⊑ a)
symbol S_Trans (s t u: Set): ε (s ⊑ t) ⇒ ε (t ⊑ u) ⇒ ε (s ⊑ u)

symbol S_Restr {a: Set} (p: η a ⇒ Bool): ε (psub p ⊑ a)

symbol S_Arr (t u1 u2: Set): ε (u1 ⊑ u2) ⇒ ε ((t ~> u1) ⊑ (t ~> u2))
symbol S_Darr (d: Set) (r1: η d ⇒ Set) (r2: η d ⇒ Set)
: ε (forall (λx, (r1 x) ⊑ (r2 x))) ⇒ ε ((arrd r1) ⊑ (arrd r2))

symbol upcast {A: Set} {B: Set}: ε (A ⊑ B) ⇒ η A ⇒ η B
definition ↑ {A} {B} ≔ upcast {A} {B}

// Maximal supertype
symbol μ: Set ⇒ Set
rule μ (psub {&T} _) → μ &T
rule μ (&T ~> &U) → &T ~> (μ &U)
// Direct super-type
symbol μ₀: Set ⇒ Set
rule μ₀ (psub {&T} _) → μ₀ &T

// Constraints
symbol π {T: Set}: η T ⇒ Bool
//rule π {psub {&T} &P} → λx: η (μ &T), (π {&T} x) ∧ (&P x)
