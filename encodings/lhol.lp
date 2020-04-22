// Encoding of λHOL
symbol Kind: TYPE
symbol Set: TYPE
symbol Bool: TYPE

set declared "θ"
set declared "η"
set declared "ε"
set declared "∀"
injective symbol θ: Kind → TYPE
injective symbol η: Set → TYPE
injective symbol ε: Bool → TYPE

constant symbol {|set|}: Kind
constant symbol bool: Set

rule θ {|set|} ↪ Set
rule η bool ↪ Bool

constant symbol ∀ {x: Set}: (η x → Bool) → Bool
constant symbol impd {x: Bool}: (ε x → Bool) → Bool
constant symbol arrd {x: Set}: (η x → Set) → Set

rule ε (∀ {$X} $P) ↪ Πx: η $X, ε ($P x)
with ε (impd {$H} $P) ↪ Πh: ε $H, ε ($P h)
with η (arrd {$X} $T) ↪ Πx: η $X, η ($T x)

constant symbol arr: Set → Set → Set
rule η (arr $X $Y) ↪ (η $X) → (η $Y)
set infix right 6 "~>" ≔ arr

set builtin "T" ≔ η
set builtin "P" ≔ ε
