// Non dependent tuples
require open personoj.lhol;

constant symbol σ: Set → Set → Set;
constant symbol cons {a: Set} {b: Set}: El a → El b → El (σ a b);
symbol car {a: Set} {b: Set}: El (σ a b) → El a;
symbol cdr {a: Set} {b: Set}: El (σ a b) → El b;
rule car (cons $x _) ↪ $x;
rule cdr (cons _ $y) ↪ $y;
