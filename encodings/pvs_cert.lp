// PVS-Cert
require open personoj.encodings.lhol

constant symbol psub {x: Set}: (El x → Bool) → Set
symbol pair: Π{t: Set} {p: El t → Bool} (m: El t), Prf (p m) → El (psub p)
symbol fst: Π{t: Set} {p: El t → Bool}, El (psub p) → El t

symbol snd {t: Set}{p: El t → Bool} (m: El (psub p)): Prf (p (fst m))

// Proof irrelevance
protected symbol ppair: Π{t: Set} {p: El t → Bool}, El t → El (psub p)

// Reduction
rule pair $X _ ↪ ppair $X
rule fst (ppair $M) ↪ $M

// Sub-typing relation
symbol subtype : Set → Set → Bool
set infix left 6 "⊑" ≔ subtype
