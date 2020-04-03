// PVS-Cert-, or ECC with only one universe
require open personoj.encodings.lhol

set declared "Σ"
constant symbol Σ (T: Utype): (η T ⇒ Utype) ⇒ Utype
constant symbol pair {T: Utype} {U: η T ⇒ Utype} (M: η T) (_: η (U M))
: η (Σ T U)

symbol fst {T: Utype} {U: η T ⇒ Utype}: η (Σ T U) ⇒ η T
rule fst (pair &M _) → &M

symbol snd {T: Utype} {U: η T ⇒ Utype} (M: η (Σ T U)): η (U (fst M))
rule snd (pair _ &N) → &N
