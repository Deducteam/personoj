require open personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans
require personoj.encodings.deferred as Deferred
require open personoj.encodings.subtype //FIXME: why must it be the last import?

symbol if {U: Set} {V: Set} (S: Set)
: ε (U ⊑ S) ⇒ ε (V ⊑ S) ⇒ Bool ⇒ η U ⇒ η V ⇒ η S

symbol lif {U: Set} {V: Set} (S: Set)
: ε (U ⊑ S) ⇒ ε (V ⊑ S) ⇒ Bool ⇒ Deferred.t U ⇒ Deferred.t V ⇒ η S

// lazy if
rule lif _ &pr _ true &t _ → ↑ &pr (Deferred.unquote &t)
rule lif _ _ &pr false _ &f → ↑ &pr (Deferred.unquote &f)
