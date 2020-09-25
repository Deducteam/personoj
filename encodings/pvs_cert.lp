// PVS-Cert
require open personoj.encodings.lhol

constant symbol psub {x: Set}: (El x → Bool) → Set
protected symbol pair': Π(t: Set) (p: El t → Bool), El t → El (psub p)
symbol fst: Π{t: Set} {p: El t → Bool}, El (psub p) → El t

symbol snd {t: Set}{p: El t → Bool} (m: El (psub p)): Prf (p (fst m))

// Proof irrelevance
definition pair {t: Set} {p: El t → Bool} (m: El t) (_: Prf (p m))
         ≔ pair' t p m

rule fst (pair' _ _ $M) ↪ $M
