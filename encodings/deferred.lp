// Encoding of deferred terms. It ought to be used by lazy operators such as IF
// or boolean operators
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans

// constant symbol t: TYPE
// symbol quote {T: Set}: η T ⇒ t
// symbol app: t ⇒ t ⇒ t
// constant symbol abs: (t ⇒ t) ⇒ t

// rule app (abs &b) &a → &b &a

// symbol unquote (T: Set): t ⇒ η T
// symbol quote_B {P: Bool}: ε P ⇒ t
// symbol unquote_B (P: Bool): t ⇒ ε P

// rule unquote (arrd &b) &f &x → unquote (&b &x) (app &f (quote &x))
//  and unquote_B (impd &b) &h &x → unquote_B (&b &x) (app &h (quote_B &x))
//  and unquote_B (forall &b) &f &x → unquote_B (&b &x) (app &f (quote &x))

// symbol if (T: Set) (p: Bool): t ⇒ t ⇒ η T
// rule if &T true &then _ → unquote &T &then
//  and if &T false _ &else → unquote &T &else

// Deferred terms carrying their type
constant symbol t: Set ⇒ TYPE
constant symbol quote {T: Set}: η T ⇒ t T
symbol unquote {T: Set}: t T ⇒ η T
rule unquote (quote &t) → &t

// Coding of applied types
constant symbol APP: Set ⇒ ∀U: Set, t U ⇒ Set
rule η (APP (arrd {&T} &bU) &T &y) → η (&bU (unquote &y))

// Quoted terms application
constant symbol app {T: Set} {S: Set}: t T ⇒ ∀y: t S, t (APP T S y)
rule unquote (app {arrd {&a} &b} {&a} &t &u) → (unquote &t) (unquote &u)

constant symbol p: Bool ⇒ TYPE
constant symbol quote_p {P: Bool}: ε P ⇒ p P
symbol unquote_p {P: Bool}: p P ⇒ ε P
