// Basically a library for vectors of `Set'
require open personoj.lhol;

// A special natural numbers type for arities
constant symbol N: TYPE;
constant symbol z: N;
constant symbol s: N → N;
symbol + : N → N → N;
rule + (s $n) $m ↪ s (+ $n $m)
with + z $m ↪ $m
with + $n z ↪ $n
with + $n (s $m) ↪ s (+ $n $m);

// Short names
symbol one ≔ s z;
symbol two ≔ s one;
symbol three ≔ s two;
symbol four ≔ s three;
symbol five ≔ s four;
symbol six ≔ s five;

require personoj.nat as Pn;
/// A constructor from PVS nat
symbol & : Pn.Nat → N;
rule & (Pn.succ $n) ↪ s (& $n)
with & Pn.zero ↪ z;

// The type of vectors of Set
constant symbol Vec : N → TYPE;
constant symbol cons {n: N}: Set → Vec n → Vec (s n);
constant symbol nil : Vec z;

/// Operations on vectors

// [rev-append v w] reverses vector [v] and appends it to [w].
injective symbol rev-append {n: N} {m: N}: Vec n → Vec m → Vec (+ m n);
rule rev-append (cons $x $tl) $v ↪ rev-append $tl (cons $x $v)
with rev-append nil $v ↪ $v;

// [rev v] reverses vector [v]
symbol rev {n: N} (v: Vec n): Vec n ≔ rev-append v nil;
assert (x1 x2 x3: Set) ⊢
  rev (cons x1 (cons x2 (cons x3 nil))) ≡ cons x3 (cons x2 (cons x1 nil));
// [append v w] appends [v] to [w].
symbol append {n: N} {m: N} (v: Vec n) (w: Vec m): Vec (+ m n) ≔
  rev-append (rev v) w;

injective symbol Tvec' (n: N) (len: N): TYPE;
rule Tvec' (s $n) $l ↪ Set → Tvec' $n $l
with Tvec' z $l ↪ Vec $l;
// [Tvec {n}] is [Π (xᵢ: Set), Vec n], the product of [n] type [Set] to [Vec n].
symbol Tvec (n: N): TYPE ≔ Tvec' n n;

/// Short constructor for vectors

injective symbol vec' (n: N) (k: N) (acc: Vec k): Tvec' n (+ k n);
rule vec' z $n $acc ↪ rev {$n} $acc
with vec' (s $n) $k $acc $e ↪ vec' $n (s $k) (cons $e $acc);
// [vec {n} x1 x2 ...] is a short constructor for a vector of {n} elements which
// are [x1], [x2] &c. like the list function in Lisp
symbol vec (n: N) ≔ vec' n z nil;

assert (x1: Set) ⊢ vec (s z) x1 ≡ cons x1 nil;
assert (x1 x2: Set) ⊢ vec (s (s z)) x1 x2 ≡ cons x1 (cons x2 nil);

// [vec->arr {n} domains range] buils the functional type that take as many
// arguments as the length of [domains] and returns a value of type [range].
injective symbol vec->arr {n: N} (v: Vec n): Set → Set;
rule vec->arr {z} _ $r ↪ $r
with vec->arr {s $n} (cons $d $tl) $r ↪ $d ~> (vec->arr {$n} $tl $r);

assert (x1 x2 x3 x4: Set) ⊢
  append (vec two x1 x2) (vec two x3 x4) ≡ vec four x1 x2 x3 x4;
assert (x1 x2 x3 x4: Set) ⊢
  rev-append (vec two x2 x1) (vec two x3 x4) ≡ vec four x1 x2 x3 x4;

/// Accessors

symbol car {n: N}: Vec n → Set;
rule car (cons $x _) ↪ $x;
symbol cdr {n: N}: Vec (s n) → Vec n;
rule cdr (cons _ $x) ↪ $x;

symbol nth {n: N}: N → Vec n → Set;
rule nth z (cons $x _) ↪ $x
with nth (s $n) (cons _ $tl) ↪ nth $n $tl;

assert (x x': Set) ⊢ nth two (vec four x x x' x) ≡ x';
assert (x x': Set) ⊢ nth z (vec four x' x x x) ≡ x';
