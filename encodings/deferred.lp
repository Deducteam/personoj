// Encoding of deferred terms. It ought to be used by lazy operators such as IF
// or boolean operators
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

constant symbol t: Set ⇒ TYPE
constant symbol p: Bool ⇒ TYPE
constant symbol quote {T: Set}: η T ⇒ t T
constant symbol quote_p {P: Bool}: ε P ⇒ p P

// Application at sort level
symbol sapp: Set ⇒ Set ⇒ Set
rule sapp (&T ~> &S) &T → &S

// Quoted terms application
constant symbol app {T: Set} {S: Set}: t T ⇒ t S ⇒ t (sapp T S)

// Unquote terms
symbol unquote {T: Set}: t T ⇒ η T
rule unquote (quote &x) → &x
rule unquote {&S} (app {&T ~> &S} {&T} &t &u) → (unquote &t) (unquote &u)

symbol unquote_p {P: Bool}: p P ⇒ ε P
