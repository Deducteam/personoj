require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol
require open personoj.encodings.tuple
require open personoj.encodings.prenex

set declared "Σ"

constant symbol Σ (t: Set): (η t → Set) → Set
symbol pair_ss {t: Set} {u: η t → Set} (m: η t): η (u m) → η (Σ t u)

symbol fst_ss {t: Set} {u: η t → Set}: η (Σ t u) → η t
symbol snd_ss {t: Set} {u: η t → Set} (m: η (Σ t u)): η (u (fst_ss m))

rule fst_ss (pair_ss $m _) ↪ $m
rule snd_ss (pair_ss _ $n) ↪ $n
