/// Encoding of λHOL
symbol Kind: TYPE
symbol Set: TYPE
symbol Bool: TYPE

set declared "∀"
injective symbol El: Set → TYPE
injective symbol Prf: Bool → TYPE

constant symbol {|set|}: Kind
constant symbol bool: Set

rule El bool ↪ Bool

constant symbol ∀ {x: Set}: (El x → Bool) → Bool
constant symbol impd {x: Bool}: (Prf x → Bool) → Bool
constant symbol arrd {x: Set}: (El x → Set) → Set

rule Prf (∀ {$X} $P) ↪ Πx: El $X, Prf ($P x)
with Prf (impd {$H} $P) ↪ Πh: Prf $H, Prf ($P h)
with El (arrd {$X} $T) ↪ Πx: El $X, El ($T x)

constant symbol arr: Set → Set → Set
rule El (arr $X $Y) ↪ (El $X) → (El $Y)
set infix right 6 "~>" ≔ arr

set builtin "T" ≔ El
set builtin "P" ≔ Prf
