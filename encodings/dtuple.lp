// Dependent tuples
require open personoj.encodings.lhol

constant symbol t (a: Set): (El a → Set) → Set
symbol cons {a: Set} {b: El a → Set} (m: El a): El (b m) → El (t a b)
symbol car {a: Set} {b: El a → Set}: El (t a b) → El a
symbol cdr {a: Set} {b: El a → Set} (m: El (t a b)): El (b (car m))
rule car (cons $m _) ↪ $m
rule cdr (cons _ $n) ↪ $n
