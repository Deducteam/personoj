require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.logical
require open personoj.encodings.equality


set declared "ℕ"
constant symbol ℕ: Set
constant symbol z: El ℕ
constant symbol s (_: El ℕ): El ℕ
constant symbol leq: El ℕ → El ℕ → Prop
set infix left 3 "≤" ≔ leq

// Agda manual
symbol p1: Prf (z ≤ s z)
symbol p2: Prf (z ≤ s z)
definition bounded (k: El ℕ) ≔ psub (λn, n ≤ k)

constant symbol slist (_: El ℕ): Set
constant symbol snil (bound: El ℕ): El (slist bound)
constant symbol scons {bound: El ℕ} (head: El (bounded bound))
                      (_: El (slist (fst head)))
              : El (slist bound)

set declared "l₁"
set declared "l₂"
definition l₁: El (slist (s z)) ≔ scons (pair z p1) (snil z)
definition l₂: El (slist (s z)) ≔ scons (pair z p2) (snil z)

theorem listeq: Prf (l₁ = l₂)
proof
  refine eq_refl l₁
qed

constant symbol even_p: El ℕ → Prop
definition even ≔ psub even_p

// Proof irrelevance without K
definition eqEven (e1 e2: El even) ≔ fst e1 = fst e2

symbol plus: El ℕ → El ℕ → El ℕ
set infix left 10 "+" ≔ plus

symbol plus_closed_even (n m: El even): Prf (even_p ((fst n) + (fst m)))

definition add (n m: El even) : El even
         ≔ pair ((fst n) + (fst m)) (plus_closed_even n m)

symbol plus_commutativity (n m: El ℕ): Prf (n + m = m + n)

theorem even_add_commutativity (n m: El even): Prf (eqEven (add n m) (add m n))
proof
  assume n m
  refine plus_commutativity (fst n) (fst m)
qed
