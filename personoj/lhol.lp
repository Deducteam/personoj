/// Encoding of λHOL
constant symbol Set: TYPE;
constant symbol Prop: TYPE;

injective symbol El: Set → TYPE;
injective symbol Prf: Prop → TYPE;

constant symbol prop: Set;
rule El prop ↪ Prop;

constant symbol ∀ {x: Set}: (El x → Prop) → Prop;
rule Prf (∀ {$X} $P) ↪ Π x: El $X, Prf ($P x);

constant symbol impd {x: Prop}: (Prf x → Prop) → Prop;
rule Prf (impd {$H} $P) ↪ Π h: Prf $H, Prf ($P h);

constant symbol arrd (x: Set): (El x → Set) → Set;
rule El (arrd $X $T) ↪ Π x: El $X, El ($T x);

constant symbol arr: Set → Set → Set;
rule El (arr $X $Y) ↪ (El $X) → (El $Y);
symbol ~> ≔ arr;
notation ~> infix right 6;

builtin "T" ≔ El;
builtin "P" ≔ Prf;
