require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical personoj.sum;

coercion #c (El (psub [$a] $p)) $t $b ↪ #c (El $a) (fst [$a] [$p] $t) $b;
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x : El a;
coercion #c $a $t (El (psub [$b] $p)) ↪ pair [$b] [$p] (#c $a $t (El $b)) _;
assert (a: Set) (p: El a → Prop) (x: El a) ⊢ x : El (psub p);

require personoj.extra.arity-tools as A;
/* All implicit arguments of the RHS must be explicited. Otherwise, they are
   considered as extra goals. This is due to the fact that there is no type
   checking of coercions yet. */
coercion #c (El (σ [A.s $l] ($a & $v))) $arg (El (σ ($b & $w))) ↪
  @^ $l $b $w
     (#c (El $a) (@head (A.s $l) (@& $l $a $v) $arg) (El $b))
     (#c (El (@σ $l $v)) (@tail $l (@& $l $a $v) $arg) (El (@σ $l $w)));
coercion #c (El (σ &nil)) $t (El (σ &nil)) ↪ $t;
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x ^^ x : El (σ (a && a));
