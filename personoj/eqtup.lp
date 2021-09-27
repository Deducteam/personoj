// Equality
// The equality of PVS is an operation from a 2-uple to bool, so we do the same
// thing here.
require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical;

symbol eq {t: Set} (_: El (σ (t & t & &nil))): Prop;
// Equality operates on the maximal supertype. It allows to profit
// from predicate subtyping for free in the propositional equality.
rule @eq (@psub $t $p) (argS $x (argS $y arg0))
   ↪ @eq $t (argS (@fst $t $p $x) (argS (@fst $t $p $y) arg0));
symbol neq {t} m ≔ ¬ (@eq t m);
