require open personoj.encodings.lhol
             personoj.encodings.deptype;

constant symbol Scheme: TYPE;
injective symbol Els: Scheme → TYPE;
constant symbol lift: Kind → Scheme;
rule Els (lift $X) ↪ Ty $X;
constant symbol ∀: (Set → Scheme) → Scheme;
rule Els (∀ $e) ↪ Π t: Set, Els ($e t);
