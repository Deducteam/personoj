// Minimal predicate logic

constant symbol Set: TYPE;
constant symbol Prop: TYPE;

injective symbol El: Set → TYPE;
injective symbol Prf: Prop → TYPE;

constant symbol ∀ {x: Set}: (El x → Prop) → Prop;
rule Prf (∀ {$X} $P) ↪ Π x: El $X, Prf ($P x);

constant symbol imp: Prop → Prop → Prop;
rule Prf (imp $P $Q) ↪ Prf $P → Prf $Q;

builtin "T" ≔ El;
builtin "P" ≔ Prf;
