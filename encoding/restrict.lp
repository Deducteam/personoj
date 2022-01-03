// Encode the operators restrict and extend. They allow to use some sort
// of contravariance on domains.
require open personoj.lhol personoj.pvs_cert personoj.logical;

symbol _sup: Set → Set;
rule _sup (@psub $a _) ↪ $a;
// [match-psub a f] returns [f p] when [a] is [psub b p].
symbol match-psub {ret: Set} (a: Set): El (((_sup a ~> prop) ~> ret) ~> ret);
rule match-psub (@psub $a $p) $f ↪ $f $p;

symbol restrict {T: Set} {S: Set} {R: Set}: El ((T ~> R) ~> S ~> R);
rule restrict {$T} {$T} $f ↪ $f;
// Computational content should be added so that restrict f x
// rewrites to f x with proper coercions inserted

symbol extend {T: Set} {S: Set} {R: Set} {d: El R}: El ((S ~> R) ~> T ~> R);
rule @extend $T $T _ $d $f ↪ $f;
rule @extend $T (@psub $T $P) _ $d $f $t
   ↪ if ($P $t)
        (λ h: Prf ($P $t), $f (pair $t h))
        (λ _, $d);
