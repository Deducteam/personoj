/// Encoding of λHOL
symbol Kind: TYPE;
symbol Set: TYPE;
symbol Prop: TYPE;

injective symbol El: Set → TYPE;
injective symbol Prf: Prop → TYPE;

constant symbol {|set|}: Kind;
constant symbol prop: Set;

rule El prop ↪ Prop;

constant symbol ∀ {x: Set}: (El x → Prop) → Prop;
constant symbol impd {x: Prop}: (Prf x → Prop) → Prop;
constant symbol arrd {x: Set}: (El x → Set) → Set;

rule Prf (∀ {$X} $P) ↪ Π x: El $X, Prf ($P x)
with Prf (impd {$H} $P) ↪ Π h: Prf $H, Prf ($P h)
with El (arrd {$X} $T) ↪ Π x: El $X, El ($T x);

constant symbol arr: Set → Set → Set;
rule El (arr $X $Y) ↪ (El $X) → (El $Y);
set infix right 6 "~>" ≔ arr;

set builtin "T" ≔ El;
set builtin "P" ≔ Prf;
