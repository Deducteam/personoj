require open personoj.lhol personoj.pvs_cert;

coercion #c (El (psub [$a] $p)) $t $b ↪ #c (El $a) (fst [$a] [$p] $t) $b;
assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ x : El a;
coercion #c $a $t (El (psub [$b] $p)) ↪ pair [$b] [$p] (#c $a $t (El $b)) _;
assert (a: Set) (p: El a → Prop) (x: El a) ⊢ x : El (psub p);
