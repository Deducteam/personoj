// Encoding of deferred terms. It ought to be used by lazy operators such as IF
// or boolean operators
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

constant symbol t: Set ⇒ TYPE
constant symbol quote {T: Set}: η T ⇒ t T

// Application at sort type
symbol sapp: Set ⇒ Set ⇒ Set
rule sapp (&T ~> &S) &T → &T

// Quoted terms application
constant symbol app {T: Set} {S: Set}: t T ⇒ t S ⇒ t (sapp T S)

// Unquote terms
symbol unquote {T: Set}: t T ⇒ η T
rule unquote (quote &x) → &x
rule unquote {&S} (app {&T ~> &S} {&T} &t &u) → (unquote &t) (unquote &u)
