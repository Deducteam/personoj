// Equality defined on tuple of arguments
require open personoj.lhol
             personoj.pvs_cert;
require personoj.tuple as T;
require open personoj.logical;

// Equality operates on the maximal supertype. It allows to profit
// from predicate subtyping for free in the propositional equality.
symbol eq {t: Set} (_: El (T.t t t)): Prop;
symbol neq {t} m ≔ ¬ (@eq t m);
rule @eq (@psub $t $p) (T.cons $x $y)
   ↪ @eq $t (T.cons (@fst $t $p $x) (@fst $t $p $y));
