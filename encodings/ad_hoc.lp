require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.subtype

// A possible encoding of ad-hoc polymorphism.
// Should be made prenex.
set declared "∀ah"
constant symbol ∀ah: Set → (Set → Set) → Set
rule η (∀ah $a $b) ↪ Π(c: Set) (h: ε (c ~ $a)) (x: η c),
                     ε (π {$a} (eqcast h (↑ x))) → η ($b c)
// TODO: work on implicitness of argument of π
