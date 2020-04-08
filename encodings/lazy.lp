require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

constant symbol delayed: Set ⇒ TYPE
constant symbol quote {T: Set}: η T ⇒ delayed T

// Application at sort type
symbol sapp: Set ⇒ Set ⇒ Set
rule sapp (&T ~> &S) &T → &T

// Quoted terms application
constant symbol app {T: Set} {S: Set}
: delayed T ⇒ delayed S ⇒ delayed (sapp T S)

// Unquote terms
symbol unquote {T: Set}: delayed T ⇒ η T
rule unquote (quote &x) → &x
rule unquote {&S} (app {&T ~> &S} {&T} &t &u) → (unquote &t) (unquote &u)
