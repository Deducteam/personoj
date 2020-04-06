require open personoj.encodings.lhol
             personoj.encodings.pvs_cert
             personoj.encodings.subtype
             personoj.encodings.equality

symbol forall_sub: Set ⇒ (Set ⇒ Set) ⇒ Set
rule η (forall_sub &T &C) → ∀U: Set, ε (U ⊑ &T) ⇒ η (&C U)
