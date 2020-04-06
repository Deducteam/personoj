// Encoding of λHOL
symbol Kind: TYPE
symbol Set: TYPE
symbol Bool: TYPE

set declared "ϕ"
set declared "η"
set declared "ε"
injective symbol ϕ: Kind ⇒ TYPE
injective symbol η: Set ⇒ TYPE
injective symbol ε: Bool ⇒ TYPE

symbol {|set|}: Kind
symbol bool: Set

rule ϕ {|set|} → Set
rule η bool → Bool

symbol forall {x: Set}: (η x ⇒ Bool) ⇒ Bool
symbol impd {x: Bool}: (ε x ⇒ Bool) ⇒ Bool
symbol arrd {x: Set}: (η x ⇒ Set) ⇒ Set

rule ε (forall {&X} &P) → ∀x: η &X, ε (&P x)
 and ε (impd {&H} &P) → ∀h: ε &H, ε (&P h)
 and η (arrd {&X} &T) → ∀x: η &X, η (&T x)

symbol arr: Set ⇒ Set ⇒ Set
rule η (arr &X &Y) → (η &X) ⇒ (η &Y)
set infix right 6 "~>" ≔ arr

set builtin "T" ≔ η
set builtin "P" ≔ ε
