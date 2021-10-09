// Dependent sum type
require open personoj.lhol;

constant symbol Σ (a: Set): (El a → Set) → Set;
symbol consd {a: Set} {b: El a → Set} (m: El a): El (b m) → El (Σ a b);
symbol card {a: Set} {b: El a → Set}: El (Σ a b) → El a;
symbol cdrd {a: Set} {b: El a → Set} (m: El (Σ a b)): El (b (card m));
rule card (consd $m _) ↪ $m;
rule cdrd (consd _ $n) ↪ $n;
