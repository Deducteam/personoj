require open personoj.lhol personoj.pvs_cert;

coerce_rule coerce (El (psub [$a] $p)) $b $t ↪ coerce (El $a) $b (fst [$a] [$p] $t);
assert (a: Set) (p: El a → Prop) (x: El (psub [a] p)) ⊢ x : El a;
coerce_rule coerce $a (El (psub [$b] $p)) $t ↪ pair [$b] [$p] (coerce $a (El $b) $t) _;
assert (a: Set) (p: El a → Prop) (x: El a) ⊢ x : El (psub p);
