// Prenex ad-hoc polymorphism
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require personoj.encodings.prenex as P
require open personoj.encodings.subtype

constant symbol forallS: Set → (Set → Scheme) → Scheme
rule P.χ (forallS $T $C) ↪ ΠU: Set, ε (U ⊑ $T) → P.χ ($C U)

constant symbol forallB: Set → (Set → Bool) → Bool
rule ε (forallB $T $C) ↪ ΠU: Set, ε (U ⊑ $T) → ε ($C U)
