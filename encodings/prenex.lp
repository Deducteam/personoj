require open personoj.encodings.lhol personoj.encodings.pvs_cert

set declared "χ"
set declared "∇"
symbol Scheme: TYPE
symbol χ: Scheme ⇒ TYPE
symbol ∇: Utype ⇒ Scheme
rule χ (∇ &X) → η &X

symbol forall_pt: (Utype ⇒ Scheme) ⇒ Scheme
rule χ (forall_pt &p) → ∀T: Utype, χ (&p T)
