// PVS-Cert-, or ECC with only one universe
require open personoj.encodings.lhol

set declared "Σ"
constant symbol Σ (T : Set): (η T → Set) → Set
constant symbol pair {T: Set} {U: η T → Set} (M: η T) (_: η (U M))
: η (Σ T U)

symbol fst {T: Set} {U: η T → Set}: η (Σ T U) → η T
rule fst (pair $M _) ↪ $M

symbol snd {T: Set} {U: η T → Set} (M: η (Σ T U)): η (U (fst M))
rule snd (pair _ $N) ↪ $N
