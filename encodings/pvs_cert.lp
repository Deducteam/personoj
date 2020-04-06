// PVS-Cert
require open personoj.encodings.lhol

constant symbol psub {x: Set}: (η x ⇒ Bool) ⇒ Set
symbol pair: ∀{t: Set} {p: η t ⇒ Bool} (m: η t), ε (p m) ⇒ η (psub p)
symbol fst: ∀{t: Set} {p: η t ⇒ Bool}, η (psub p) ⇒ η t

symbol snd {t: Set}{p: η t ⇒ Bool} {m: η (psub p)}: ε (p (fst m))

// Proof irrelevance
protected symbol ppair: ∀{t: Set} {p: η t ⇒ Bool}, η t ⇒ η (psub p)

// Reduction
rule pair &X _ → ppair &X
rule fst (ppair &M) → &M
