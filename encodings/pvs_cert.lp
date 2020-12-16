// PVS-Cert
require open personoj.encodings.lhol

set declared "μ"

constant symbol psub {x: Set}: (El x → Prop) → Set
protected symbol pair': Π(t: Set) (p: El t → Prop), El t → El (psub p)
symbol fst: Π{t: Set} {p: El t → Prop}, El (psub p) → El t

symbol snd {t: Set}{p: El t → Prop} (m: El (psub p)): Prf (p (fst m))

// Proof irrelevance
symbol pair {t: Set} {p: El t → Prop} (m: El t) (_: Prf (p m))
     ≔ pair' t p m

rule fst (pair' _ _ $M) ↪ $M

symbol μ (_: Set): Set
rule μ (@psub $t _) ↪ μ $t
with μ prop ↪ prop
