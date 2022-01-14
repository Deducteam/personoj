// FIXME: implicit arguments must be avoided in the definition of coercions
// because they are not type checked properly.
require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical personoj.sum;

coercion #c $a $t $a ↪ $t;

coercion #c (El (psub [$a] $p)) $t (El $b) ↪ #c (El $a) (fst [$a] [$p] $t) (El $b);
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x : El a;
coercion #c (El $a) $t (El (psub [$b] $p)) ↪ pair [$b] [$p] (#c (El $a) $t (El $b)) _;
assert (a: Set) (p: El a → Prop) (x: El a) ⊢ x : El (psub p);

// Coercion:

require personoj.extra.arity-tools as A;
assert (a: Set) (v: SVec _) ⊢ El (σ (a & v)): TYPE;
coercion #c (El (σ ($a & $v))) $arg (El (σ ($b & $w))) ↪
  (#c (El $a) (head $arg) (El $b)) ^ (#c (El (σ $v)) (tail $arg) (El (σ $w)));
// This coercion should be superseded by the first one
coercion #c (El (σ &nil)) $t (El (σ &nil)) ↪ $t;
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x ^^ x : El (σ (a && a));

// The following coercion is redundant with 'tup-cons'

// The following rule does not typecheck because of the last pre requisite
// coercion "dtuple"
//  (a0: Set) (b0: El a0 → Set) (a1: Set) (b1: El a1 → Set) (t: El (Σ a0 b0)) ⊢
//     @consd a0 b0 $c[@card a0 b0 t] $d[@cdrd a0 b0 t]: El (Σ a1 b1)
//  on 5
// with c : El a0 → El a1
// with d : El (b0 (@card a0 b0 t)) → El (b1 $c[@card a0 b0 t]);
