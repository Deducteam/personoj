require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

set declared "μ"
set declared "μ₀"
set declared "π"
set declared "↑"
set declared "↓"

// Maximal supertype
constant symbol μ: Set → Set
rule η (μ (psub {$T} _)) ↪ η (μ $T)
with η (μ ($T ~> $U)) ↪ η ($T ~> (μ $U))
with η (μ (μ $T)) ↪ η (μ $T) // FIXME: can be proved

symbol π {T: Set}: η (μ T) → Bool

constant symbol maxcast {t: Set}: η t → η (μ t)
constant symbol downcast {t: Set} (x: η (μ t)): ε (π x) → η t
definition ↑ {t} ≔ maxcast {t}
definition ↓ {t} ≔ downcast {t}

rule ε (π {$t ~> $u}) ↪ ε (λx: η $t → η (μ $u), forall (λy, π (x y)))
with ε (π {psub {$t} $a}) ↪ ε (λx: η (μ $t), (π x) ∧ (λy: ε (π x), $a (↓ x y)))
