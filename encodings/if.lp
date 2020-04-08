require open personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.subtype

symbol if {U: Set} {V: Set} (S: Set)
: ε (U ⊑ S) ⇒ ε (V ⊑ S) ⇒ Bool ⇒ η U ⇒ η V ⇒ η S
