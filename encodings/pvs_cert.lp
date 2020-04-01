// PVS-Cert
require open personoj.encodings.lhol

constant symbol psub {x: Utype}: (η x ⇒ Ubool) ⇒ Utype
symbol pair: ∀{t: Utype} {p: η t ⇒ Ubool} (m: η t), ε (p m) ⇒ η (psub p)
symbol fst: ∀{t: Utype} {p: η t ⇒ Ubool}, η (psub p) ⇒ η t

symbol snd {t: Utype}{p: η t ⇒ Ubool} {m: η (psub p)}: ε (p (fst m))

// Proof irrelevance
protected symbol ppair: ∀{t: Utype} {p: η t ⇒ Ubool}, η t ⇒ η (psub p)

// Reduction
rule pair &X _ → ppair &X
rule fst (ppair &M) → &M
