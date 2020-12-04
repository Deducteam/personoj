// Non dependent tuples
require open personoj.encodings.lhol

constant symbol t: Set → Set → Set
symbol cons {a: Set} {b: Set}: El a → El b → El (t a b)
symbol car {a: Set} {b: Set}: El (t a b) → El a
symbol cdr {a: Set} {b: Set}: El (t a b) → El b
rule car (cons $x _) ↪ $x
rule cdr (cons _ $y) ↪ $y
