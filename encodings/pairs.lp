require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol
require open personoj.encodings.tuple
require open personoj.encodings.prenex

set declared "Σ" // Dependent pairs
set declared "σ" // Non dependent pairs
set declared "σcons"
set declared "σfst"
set declared "σsnd"

constant symbol σ: Set → Set → Set
symbol σcons {t} {u}: El t → El u → El (σ t u)
symbol σfst {t} {u}: El (σ t u) → El t
rule σfst (σcons $x _) ↪ $x
symbol σsnd {t} {u}: El (σ t u) → El u
rule σsnd (σcons _ $y) ↪ $y

constant symbol Σ (t: Set): (El t → Set) → Set
symbol pair_ss {t: Set} {u: El t → Set} (m: El t): El (u m) → El (Σ t u)

symbol fst_ss {t: Set} {u: El t → Set}: El (Σ t u) → El t
symbol snd_ss {t: Set} {u: El t → Set} (m: El (Σ t u)): El (u (fst_ss m))

rule fst_ss (pair_ss $m _) ↪ $m
rule snd_ss (pair_ss _ $n) ↪ $n
