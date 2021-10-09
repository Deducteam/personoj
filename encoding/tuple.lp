/* Non dependent tuples, or non dependent telescopes. Should be supereseded by
   general telescopes. */
require open personoj.lhol;
require open personoj.extra.arity-tools;

constant symbol SVec : N → TYPE;
injective symbol & {n: N}: Set → SVec n → SVec (s n);
constant symbol &nil : SVec z;
notation & infix right 10;

constant symbol σ {n: N}: SVec n → Set;

symbol SVmap {n: N}: (Set → Set) → SVec n → SVec n;
rule SVmap _ &nil ↪ &nil
with SVmap $f ($x & $y) ↪ ($f $x) & (SVmap $f $y);

/* [mkarr tele ret] creates the type [tele₀ ~> tele₁ ~> ... ~> ret]. */
injective symbol mkarr {n: N}: SVec n → Set → Set;
rule mkarr &nil $Ret ↪ $Ret
with mkarr ($X & $Q) $Ret ↪ arr $X (mkarr $Q $Ret);

/* [mkarr* v] creates the product type [El v₀ → El v₁ → ... → Set]. */
symbol mkarr* {n: N}: SVec n → TYPE;
rule mkarr* &nil ↪ Set
with mkarr* ($x & $y) ↪ El $x → mkarr* $y;

injective symbol ^ {n: N} {a: Set} {v: SVec n}:
  El a → El (σ v) → El (σ (a & v));
constant symbol ^nil : El (σ &nil);
notation ^ infix right 10;

symbol match {l: N} {v: SVec l} {ret: Set} (arg: El (σ v)):
  El (mkarr v ret) → El ret;
rule match {z} {&nil} ^nil $e ↪ $e
with @match _ _ $Ret ($x ^ $y) $f ↪ @match _ _ $Ret $y ($f $x);

/* [match* t f] applied function [f] that yields a type to the components of
   [t]. */
symbol match* {l: N} {v: SVec l} (arg: El (σ v)): mkarr* v → Set;
rule match* ^nil $e ↪ $e
with match* ($x ^ $y) $f ↪ match* $y ($f $x);

/// Standard accessors

symbol car {n: N}: SVec n → Set;
rule car ($x & _) ↪ $x;
symbol cdr {n: N}: SVec (s n) → SVec n;
rule cdr (_ & $x) ↪ $x;
symbol cadr {n: N} (v: SVec (s n)): Set ≔ car (cdr v);

// Surjective pairing for SetVec
rule (car $x) & (cdr $x) ↪ $x;

symbol head {l: N} {v: SVec l} (arg: El (σ v)): El (car v);
rule head ($x ^ _) ↪ $x;
symbol tail {l: N} {v: SVec (s l)}: El (σ v) → El (σ (cdr v));
rule tail (_ ^ $y) ↪ $y;

// Surjective pairing for tuples
rule (head $x) ^ (tail $x) ↪ $x;

/// Accessors

/* [nth n v arg] returns the [n]th element of vector [v].
   NOTE: the argument [arg] is not used in the computation, but we need it to
   have the same interface as telescopes. */
symbol nth {l: N} (_: N) (v: SVec l) (_: El (σ v)): Set;
rule nth z ($x & _) _ ↪ $x
with nth (s $n) (_ & $tl) $x ↪ nth $n $tl (tail $x);
// NOTE we must take care to be able to reduce even if the argument is a
// variable.

symbol proj {l: N} {v: SVec l} (n: N) (arg: El (σ v)): El (nth n v arg);
rule proj z ($x ^ _) ↪ $x
with proj (s $n) (_ ^ $Y) ↪ proj $n $Y;

/// Convenience

/* [a && b] creates the telescope of two (independant) codes. */
symbol && (a b: Set) ≔ a & b & &nil;
notation && infix 11;
assert (X: Set) ⊢ X & X && X ≡ X & (X && X);

/* [x ^^ y] creates a tuple of two elements with [x] and [y]. */
symbol ^^ {a: Set} {b: Set} (x: El a) (y: El b) ≔ x ^ y ^ ^nil;
notation ^^ infix 11;
