require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

constant symbol delayed: Set ⇒ TYPE
constant symbol quote {T: Set}: η T ⇒ delayed T

constant symbol s_app: Set ⇒ Set ⇒ Set

constant symbol app {T: Set} {S: Set}
: delayed T ⇒ delayed S ⇒ delayed (s_app T S)

symbol unquote {T: Set}: delayed T ⇒ η T

rule unquote (quote &x) → &x
rule unquote {&S} (app {&T ~> &S} {&T} &t &u) → (unquote &t) (unquote &u)
