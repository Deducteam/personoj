/// Prenex polymorphism in sorts of type ‘Set’
require personoj.encodings.lhol as lhol;

constant symbol Scheme: TYPE;
injective symbol El: Scheme → TYPE;
constant symbol lift: lhol.Set → Scheme;
rule El (lift $X) ↪ lhol.El $X;

constant symbol ∀: (lhol.Set → Scheme) → Scheme;
rule El (∀ $e) ↪ Π a: lhol.Set, El ($e a);
