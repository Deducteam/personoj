// Integers
// This module can be used as follows
// ```
// require int;
// symbol Int ≔ int.Int;
// symbol int#o ≔ Int.int#o;
// ```
require open personoj.lhol personoj.logical personoj.nat;

symbol Int: TYPE;

constant symbol int#o: Set;
rule El int#o ↪ Int;

constant symbol positive (_: Nat): Int; // This could be a coercion
symbol negative (_: Nat): Int;
rule negative 0 ↪ positive 0;
// Keep only one number zero

// TODO one would probably like to eliminate the constrctors `positive` and
// `negative` into pair coercions
