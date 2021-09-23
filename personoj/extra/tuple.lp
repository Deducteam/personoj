// Another take on tuples as heterogeneous lists of fixed length
require open personoj.lhol;
require personoj.extra.arity-tools as A;

symbol +2 ≔ A.+ A.two;

/* [σ {n} v] creates the tuple type of [n] + 2 elements. */
constant symbol σ {n: A.N}: A.Vec (+2 n) → Set;

constant symbol cons {n: A.N} {a: Set} {v: A.Vec (+2 n)}:
  El a → El (σ {n} v) → El (σ {A.s n} (A.cons a v));
constant symbol double {a: Set} {b: Set}:
  El a → El b → El (σ (A.vec A.two a b));

symbol nth {n: A.N} {v: A.Vec (+2 n)} (k: A.N): El (σ v) → El (A.nth k v);
rule @nth A.z _ A.z (double $x _) ↪ $x
with @nth A.z _ (A.s A.z) (double _ $y) ↪ $y
with @nth (A.s $n) _ (A.s $k) (cons $x $tl) ↪ nth $k $tl
with @nth _ _ A.z (cons $x _) ↪ $x;

assert (x: Set) (e1 e2 e3: El x) ⊢ nth A.z (cons e1 (double e2 e3)) ≡ e1;
assert (x: Set) (e1 e2 e3: El x) ⊢ nth A.one (cons e1 (double e2 e3)) ≡ e2;
assert (x: Set) (e1 e2 e3: El x) ⊢ nth A.two (cons e1 (double e2 e3)) ≡ e3;

// [match f t] applies function [f] on each element of tuple [t]
symbol match {n: A.N} {v: A.Vec (+2 n)} {r: Set}: El (A.vec->arr v r) → El (σ v) → El r;
rule match $f (cons $x $tl) ↪ match ($f $x) $tl
with match $f (double $x $y) ↪ $f $x $y;

assert (x: Set) (e1 e2 e3: El x) ⊢ match (λ x1 x2 x3, x2) (cons e1 (double e2 e3)) ≡ e2;

/// Functions on tuples

/* [rev-append t u] appends [rev t] to [u]. */
injective symbol rev-append {n: A.N} {m: A.N} {v: A.Vec (+2 n)} {w: A.Vec (+2 m)}:
  El (σ v) → El (σ w) → El (σ (A.rev-append v w));
rule rev-append (cons $hd $tl) $acc ↪ rev-append $tl (cons $hd $acc)
with rev-append (double $x $y) $acc ↪ cons $y (cons $x $acc);

/* [rev t] reverses [t]. */
injective symbol rev {n: A.N} {v: A.Vec (+2 n)}: El (σ v) → El (σ (A.rev v));
rule rev (double $x $y) ↪ double $y $x
with rev (cons $x (double $y $z)) ↪ cons $z (double $y $x)
with rev (cons $x (cons $y $tl)) ↪ rev-append $tl (double $y $x);
assert (x: Set) (e1 e2 e3: El x) ⊢ rev (cons e1 (double e2 e3)) ≡ (cons e3 (double e2 e1));

/// Short constructors

/* Let [m = n + 2] and [v = x₁ ... xₘ], then [mkσ/ty {n} v] is the arrow type
   quantifying over [xᵢ] to yield the tuple type [σ x₁ ... xₘ]. */
symbol mkσ/ty {n: A.N} (v: A.Vec (+2 n)): Set ≔ A.vec->arr v (σ v);
assert (x1 x2 x3: Set) ⊢ mkσ/ty (A.vec A.three x1 x2 x3) ≡
x1 ~> x2 ~> x3 ~> σ (A.vec A.three x1 x2 x3);

//flag "print_implicits" on;
// [append-mkσ {n} {m} {v} {w} t x1 ... x(n+2)] builds the tuple made with [t]
// reversed and appended to [x1 ... x(n+2)].
symbol rappend-mkσ {n: A.N} {n/acc: A.N} {v: A.Vec (+2 n)} {v/acc: A.Vec (+2 n/acc)}:
  El (σ {n/acc} v/acc) → El (A.vec->arr {+2 n} v (σ {+2 (A.+ n n/acc)} (A.rev-append v/acc v)));
rule rappend-mkσ {A.z} {_} {A.cons _ (A.cons _ A.nil)} {_} $acc $x $y ↪
     rev-append $acc (double $x $y);
rule rappend-mkσ {A.s $n} {$n/acc} {A.cons _ $tl} {_} $acc $x ↪
     rappend-mkσ {_} {A.s $n/acc} {$tl} {_} (cons $x $acc);

/* [mkσ {n} v e₁ ... eₘ] where [m = n + 2] builds the tuple made with element
   [e₁] up to [eₘ]. Vector [v] contains the types of the [eᵢ]. */
symbol mkσ {n} (v: A.Vec (+2 n)): El (mkσ/ty v);
rule mkσ {A.z} (A.cons _ (A.cons _ A.nil)) $ex $ey ↪ double $ex $ey
with mkσ {A.s A.z} (A.cons _ (A.cons _ (A.cons _ A.nil))) $ex $ey $ez ↪
     cons $ex (double $ey $ez)
with mkσ {A.s (A.s $n)} (A.cons $x (A.cons $y $tl)) $ex $ey ↪
     rappend-mkσ {$n} {_} {$tl} {A.cons $y (A.cons $x A.nil)} (double {$y} {$x} $ey $ex);

assert (t: Set) (e1 e2 e3: El t) ⊢ mkσ (A.vec A.three t t t) e1 e2 e3 ≡ cons e1 (double e2 e3);
