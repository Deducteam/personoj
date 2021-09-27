// Basically a library for vectors of `Set'
require open personoj.lhol;

// A special natural numbers type for arities
constant symbol N: TYPE;
constant symbol z: N;
constant symbol s: N → N;
symbol + : N → N → N;
rule + (s $n) $m ↪ s (+ $n $m)
with + z $m ↪ $m
with + $n z ↪ $n
with + $n (s $m) ↪ s (+ $n $m);

// Short names
symbol one ≔ s z;
symbol two ≔ s one;
symbol three ≔ s two;
symbol four ≔ s three;
symbol five ≔ s four;
symbol six ≔ s five;

require personoj.nat as Pn;
/// A constructor from PVS nat
injective symbol & : Pn.Nat → N;
rule & (Pn.succ $n) ↪ s (& $n)
with & Pn.zero ↪ z;
