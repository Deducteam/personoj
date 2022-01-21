/* Some functions on vectors of type Set */
require open personoj.lhol;
require open personoj.extra.arity-tools;

// The type of vectors of Set
constant symbol T : N → TYPE;
constant symbol cons [n: N]: Set → T n → T (s n);
constant symbol nil : T z;

/// Operations on vectors

// [rev-append v w] reverses vector [v] and appends it to [w].
injective symbol rev-append [n: N] [m: N]: T n → T m → T (+ m n);
rule rev-append (cons $x $tl) $v ↪ rev-append $tl (cons $x $v)
with rev-append nil $v ↪ $v;

// [rev v] reverses vector [v]
symbol rev [n: N] (v: T n): T n ≔ rev-append v nil;
assert (x1 x2 x3: Set) ⊢
  rev (cons x1 (cons x2 (cons x3 nil))) ≡ cons x3 (cons x2 (cons x1 nil));
// [append v w] appends [v] to [w].
symbol append [n: N] [m: N] (v: T n) (w: T m): T (+ m n) ≔
  rev-append (rev v) w;

// [MkSetArr' ar len] creates [Set → Set → ... → T len] of arity [n]
injective symbol MkSetArr: N → N → TYPE;
rule MkSetArr (s $ar) $l ↪ Set → MkSetArr $ar $l
with MkSetArr z $l ↪ T $l;

/// Short constructor for vectors

injective symbol vec' (n: N) (k: N) (acc: T k): MkSetArr n (+ k n);
rule vec' z $n $acc ↪ rev [$n] $acc
with vec' (s $n) $k $acc $e ↪ vec' $n (s $k) (cons $e $acc);
// [vec [n] x1 x2 ...] is a short constructor for a vector of [n] elements which
// are [x1], [x2] &c. like the list function in Lisp
symbol vec (n: N) ≔ vec' n z nil;

assert (x1: Set) ⊢ vec (s z) x1 ≡ cons x1 nil;
assert (x1 x2: Set) ⊢ vec (s (s z)) x1 x2 ≡ cons x1 (cons x2 nil);

/* [vec->arr [n] domains range] buils the functional type that take as many
   arguments as the length of [domains] and returns a value of type [range].
   Dependent-safe */
injective symbol mkarr [n: N] (v: T n): Set → Set;
rule mkarr [z] _ $r ↪ $r
with mkarr [s $n] (cons $d $tl) $r ↪ $d ~> (mkarr [$n] $tl $r);

assert (x1 x2 x3 x4: Set) ⊢
  append (vec two x1 x2) (vec two x3 x4) ≡ vec four x1 x2 x3 x4;
assert (x1 x2 x3 x4: Set) ⊢
  rev-append (vec two x2 x1) (vec two x3 x4) ≡ vec four x1 x2 x3 x4;

/// Accessors

symbol car [n: N]: T n → Set;
rule car (cons $x _) ↪ $x;
symbol cdr [n: N]: T (s n) → T n;
rule cdr (cons _ $x) ↪ $x;
symbol cadr [n: N] (v: T (s n)): Set ≔ car (cdr v);

symbol nth [n: N]: N → T n → Set;
rule nth z (cons $x _) ↪ $x
with nth (s $n) (cons _ $tl) ↪ nth $n $tl;

assert (x x': Set) ⊢ nth two (vec four x x x' x) ≡ x';
assert (x x': Set) ⊢ nth z (vec four x' x x x) ≡ x';
