require open personoj.lhol
             personoj.extra.deptype;

constant symbol Scheme: TYPE;
injective symbol El: Scheme → TYPE;
constant symbol lift: Kind → Scheme;
rule El (lift $X) ↪ Ty $X;
constant symbol ∀: (Set → Scheme) → Scheme;
rule El (∀ $e) ↪ Π t: Set, El ($e t);
