/* Non dependent tuples, or non dependent telescopes. Should be supereseded by
   general telescopes. */
require open personoj.lhol;
require open personoj.extra.arity-tools;

constant symbol SVec : N → TYPE;
constant symbol & {n: N}: Set → SVec n → SVec (s n);
constant symbol &nil : SVec z;
notation & infix right 10;

constant symbol σ {n: N}: SVec n → Set;

symbol SVmap {n: N}: (Set → Set) → SVec n → SVec n;
rule SVmap $f &nil ↪ &nil
with SVmap $f ($x & $y) ↪ ($f $x) & (SVmap $f $y);

/* [mkarr tele ret] creates the type [tele₀ ~> tele₁ ~> ... ~> ret]. */
injective symbol mkarr {n: N}: SVec n → Set → Set;
rule mkarr &nil $Ret ↪ $Ret
with mkarr ($X & $Q) $Ret ↪ arr $X (mkarr $Q $Ret);

constant symbol ^ {n: N} {a: Set} {v: SVec n}:
  El a → El (σ v) → El (σ (a & v));
constant symbol ^nil : El (σ &nil);
notation ^ infix right 10;

symbol match {l: N} {v: SVec l} (ret: Set) (arg: El (σ v)):
  El (mkarr v ret) → El ret;
rule match {z} {&nil} _ ^nil $e ↪ $e
with match $Ret ($x ^ $y) $f ↪ match $Ret $y ($f $x);

/// Accessors

/* [nth n v arg] returns the [n]th element of vector [v].
   NOTE: the argument [arg] is not used in the computation, but we need it to
   have the same interface as telescopes. */
symbol nth {n: N} (_: N) (v: SVec n) (_: El (σ v)): Set;
rule nth z ($x & _) _ ↪ $x
with nth (s $n) (_ & $tl) (_ ^ $y) ↪ nth $n $tl $y;

symbol car {n: N}: SVec n → Set;
rule car ($x & _) ↪ $x;
symbol cdr {n: N}: SVec (s n) → SVec n;
rule cdr (_ & $x) ↪ $x;
symbol cadr {n: N} (v: SVec (s n)): Set ≔ car (cdr v);

symbol argn {l: N} {v: SVec l} (n: N) (arg: El (σ v)): El (nth n v arg);
rule argn z ($x ^ _) ↪ $x
with argn (s $n) (_ ^ $Y) ↪ argn $n $Y;

/// Low level accessors

symbol head {l: N} {v: SVec l} (arg: El (σ v)): El (car v);
rule head ($x ^ _) ↪ $x;
symbol tail {l: N} {v: SVec (s l)}: El (σ v) → El (σ (cdr v));
rule tail (_ ^ $y) ↪ $y;
