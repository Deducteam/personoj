/// Encoding of λHOL
constant symbol Set: TYPE;
constant symbol Prop: TYPE;

// WARNING: El and Prf are not injective.
injective symbol El: Set → TYPE;
injective symbol Prf: Prop → TYPE;

constant symbol prop: Set;
rule El prop ↪ Prop;

constant symbol ∀ [x: Set]: (El x → Prop) → Prop;
rule Prf (∀ [$X] $P) ↪ Π x: El $X, Prf ($P x);

constant symbol ⇒ (p: Prop): (Prf p → Prop) → Prop; notation ⇒ infix right 2;
rule Prf ($P ⇒ $Q) ↪ Π h: Prf $P, Prf ($Q h);

constant symbol arrd (x: Set): (El x → Set) → Set;
rule El (arrd $X $T) ↪ Π x: El $X, El ($T x);
// To have injectivity wrt. El, the above rule may be replaced by
// rule El (arrd $X (\x, $T[x])) ↪ Π y: El $X, El ($T[y]);

constant symbol arr: Set → Set → Set;
rule El (arr $X $Y) ↪ (El $X) → (El $Y);
symbol ~> ≔ arr;
notation ~> infix right 6;

builtin "T" ≔ El;
builtin "P" ≔ Prf;
