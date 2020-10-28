require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol


set declared "ℕ"
constant symbol ℕ: Set
constant symbol z: El ℕ
constant symbol s (_: El ℕ): El ℕ
constant symbol leq: El ℕ → El ℕ → Bool
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

// Proof irrelevance without K
// We need the following axiom for pairs
symbol app_pair {a b: Set} (x y: El a) (p: El a → Bool)
                (hx: Prf (p x)) (hy: Prf (p y)) (_: Prf (x = y))
     : Prf (@pair _ p x hx = @pair _ p y hy)

symbol plus: El ℕ → El ℕ → El ℕ
set infix left 10 "+" ≔ plus

symbol even_p: El ℕ → Bool
definition even ≔ psub even_p

symbol plus_closed_even (n m: El even): Prf (even_p ((fst n) + (fst m)))

definition add (n m: El even) : El even
         ≔ pair ((fst n) + (fst m)) (plus_closed_even n m)

symbol plus_commutativity (n m: El ℕ): Prf (n + m = m + n)

theorem even_add_commutativity (n m: El even): Prf (add n m = add m n)
proof
  assume n m
  refine app_pair (fst n + fst m) (fst m + fst n) even_p _ _ _
  refine ℕ
  // fst n + fst m is even
  refine plus_closed_even n m
  // fst m + fst n is even
  refine plus_closed_even m n
  // fst n + m = fst m + n
  refine plus_commutativity (fst n) (fst m)
qed
