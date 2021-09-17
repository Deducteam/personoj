require open personoj.lhol personoj.logical;
symbol Nat: TYPE;

constant symbol zero: Nat;       builtin "0" ≔ zero;
constant symbol succ: Nat → Nat; builtin "+1" ≔ succ;

symbol eqnat: Nat → Nat → Prop;
rule eqnat (succ $n) (succ $m) ↪ eqnat $n $m
with eqnat zero (succ _) ↪ false
with eqnat (succ _) zero ↪ false
with eqnat zero zero ↪ true;

symbol lenat: Nat → Nat → Prop;
rule lenat zero zero ↪ false
with lenat (succ _) zero ↪ false
with lenat zero (succ _) ↪ true
with lenat (succ $n) (succ $m) ↪ lenat $n $m;

// Recursor on integers
symbol nrec {t: Set} : Nat → El t → (Nat → El t → El t) → El t;
rule nrec {$t} (succ $n) $base $f ↪ $f $n (nrec {$t} $n $base $f);
rule nrec zero $base _ ↪ $base;

// We introduce an object level Nat to have polymorphism on the recursor.
symbol nat/o: Set;
rule El nat/o ↪ Nat;
symbol + (sn m: Nat) ≔ nrec sn m (λ _ npm, succ npm); notation + infix 6;
