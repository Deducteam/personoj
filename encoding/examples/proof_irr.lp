require open personoj.lhol personoj.pvs_cert personoj.logical personoj.eq personoj.alt.eq;

notation = infix 2;

constant symbol ℕ: Set;
constant symbol z: El ℕ;
constant symbol s (_: El ℕ): El ℕ;
constant symbol ≤: El ℕ → El ℕ → Prop;
notation ≤ infix 3;

// Agda manual
symbol p1: Prf (z ≤ s z);
symbol p2: Prf (z ≤ s z);
symbol bounded (k: El ℕ) ≔ psub (λ n, n ≤ k);

constant symbol slist (_: El ℕ): Set;
constant symbol snil (bound: El ℕ): El (slist bound);
constant symbol scons [bound: El ℕ] (head: El (bounded bound))
                      (_: El (slist (fst head)))
              : El (slist bound);

symbol l₁: El (slist (s z)) ≔ scons (pair z p1) (snil z);
symbol l₂: El (slist (s z)) ≔ scons (pair z p2) (snil z);

opaque symbol listeq: Prf (l₁ = l₂) ≔ refl l₁;
// begin
//   refine eq_refl l₁
// end

constant symbol even_p: El ℕ → Prop;
symbol even ≔ psub even_p;

// Proof irrelevance without K
symbol eqEven (e1 e2: El even) ≔ fst e1 = fst e2;

symbol +: El ℕ → El ℕ → El ℕ;
notation + infix left 10;

symbol plus_closed_even (n m: El even): Prf (even_p ((fst n) + (fst m)));

symbol add (n m: El even) : El even
     ≔ pair ((fst n) + (fst m)) (plus_closed_even n m);

symbol plus_commutativity (n m: El ℕ): Prf (n + m = m + n);

opaque symbol even_add_commutativity (n m: El even)
            : Prf (eqEven (add n m) (add m n))
            ≔
begin
  assume n m;
  refine plus_commutativity (fst n) (fst m);
end;
