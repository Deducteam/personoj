require open personoj.lhol personoj.logical;
symbol Nat: TYPE;

constant symbol zero: Nat;       builtin "0" ≔ zero;
constant symbol succ: Nat → Nat; builtin "+1" ≔ succ;

symbol eqnat: Nat → Nat → Prop;
rule eqnat (succ $n) (succ $m) ↪ eqnat $n $m
with eqnat zero (succ _) ↪ false
with eqnat (succ _) zero ↪ false
with eqnat zero zero ↪ true;
