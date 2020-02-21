require open encodings.cert_f prelude.cert_f.booleans

symbol if {T: Term uType} : Term uProp ⇒ Term T ⇒ Term T ⇒ Term T

rule if true &t  _ → &t
 and if false _ &f → &f
