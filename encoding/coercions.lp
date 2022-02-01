require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical personoj.sum;

coercion #c (El (psub [$a] $p)) $t $b ↪ #c (El $a) (fst [$a] [$p] $t) $b;
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x : El a;
coercion #c $a $t (El (psub [$b] $p)) ↪ pair [$b] [$p] (#c $a $t (El $b)) _;
assert (a: Set) (p: El a → Prop) (x: El a) ⊢ x : El (psub p);

require personoj.extra.arity-tools as A;
coercion #c (El (σ ($a & $v))) $arg (El (σ ($b & $w))) ↪
  (#c (El $a) (head $arg) (El $b)) ^ (#c (El (σ $v)) (tail $arg) (El (σ $w)));
coercion #c (El (σ &nil)) $t (El (σ &nil)) ↪ $t;
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x ^^ x : El (σ (a && a));
