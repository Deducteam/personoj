// Encoding of λHOL
symbol Ukind: TYPE
symbol Utype: TYPE
symbol Ubool: TYPE

set declared "ϕ"
set declared "η"
set declared "ε"
injective symbol ϕ: Ukind ⇒ TYPE
injective symbol η: Utype ⇒ TYPE
injective symbol ε: Ubool ⇒ TYPE

symbol ctype: Ukind
symbol cbool: Utype

rule ϕ ctype → Utype
rule η cbool → Ubool

symbol forall {x: Utype}: (η x ⇒ Ubool) ⇒ Ubool
symbol impd {x: Ubool}: (ε x ⇒ Ubool) ⇒ Ubool
symbol arrd {x: Utype}: (η x ⇒ Utype) ⇒ Utype

rule ε (forall {&X} &P) → ∀x: η &X, ε (&P x)
 and ε (impd {&H} &P) → ∀h: ε &H, ε (&P h)
 and η (arrd {&X} &T) → ∀x: η &X, η (&T x)

symbol arr: Utype ⇒ Utype ⇒ Utype
rule η (arr &X &Y) → (η &X) ⇒ (η &Y)
set infix right 6 "~>" ≔ arr
