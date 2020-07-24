// PVS-Cert-, or ECC with only one universe
require open personoj.encodings.lhol

set declared "Σ"
constant symbol Σ (T : Set): (El T → Set) → Set
constant symbol pair {T: Set} {U: El T → Set} (M: El T) (_: El (U M))
              : El (Σ T U)

symbol fst {T: Set} {U: El T → Set}: El (Σ T U) → El T
rule fst (pair $M _) ↪ $M

symbol snd {T: Set} {U: El T → Set} (M: El (Σ T U)): El (U (fst M))
rule snd (pair _ $N) ↪ $N
