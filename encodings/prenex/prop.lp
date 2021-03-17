/// Prenex polymorphism in objects of type ‘Prop’

require open personoj.encodings.lhol;

constant symbol ∀: (Set → Prop) → Prop;
rule Prf (∀ $P) ↪ Π x: Set, Prf ($P x);
