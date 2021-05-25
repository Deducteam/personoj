// Non dependent tuples
require open personoj.lhol;

constant symbol σ: Set → Set → Set;
constant symbol cons {a: Set} {b: Set}: El a → El b → El (σ a b);
symbol car {a: Set} {b: Set}: El (σ a b) → El a;
symbol cdr {a: Set} {b: Set}: El (σ a b) → El b;
rule car (cons $x _) ↪ $x;
rule cdr (cons _ $y) ↪ $y;

require open personoj.nat;

// [Proj n l t] returns the element at index [n] of tuple [t] of length [l].
symbol Proj : Nat → Nat → Set → TYPE;
rule Proj 0 0 $t ↪ El $t
with Proj 0 (succ _) (σ $t _) ↪ El $t
with Proj (succ $n) (succ $l) (σ _ $u) ↪ Proj $n $l $u;

// [proj n l e] returns the element at index [n] of list (made of pairs) [e] of
// length [l], where [length (σ _ B) = 1 + length B] and [length t = 0] if [t]
// is not a pair.
symbol proj {A: Set} {B: Set} (n: Nat) (m: Nat) (_: El (σ A B)):
  Proj n m (σ A B);
rule proj 0 (succ _) $e ↪ car $e
with proj 1 1 $e ↪ cdr $e
with @proj $A (σ $B1 $B2) (succ $n) (succ $l) $e
   ↪ @proj $B1 $B2 $n $l (@cdr $A (σ $B1 $B2) $e);

assert (T: Set) (U: Set) ⊢ Proj 1 1 (σ T U) ≡ El U;
assert (T: Set) (U: Set) ⊢ Proj 0 1 (σ T U) ≡ El T;
assert (T: Set) (U: Set) (V: Set) ⊢ Proj 2 2 (σ T (σ U V)) ≡ El V;
assert (T: Set) (e: El T) (e': El T) ⊢ proj 1 1 (cons e e') ≡ e';
assert (T: Set) (e: El T) (e': El T) ⊢ proj 0 1 (cons e e') ≡ e;
assert (T: Set) (U: Set) (V: Set) (et: El T) (eu: El U) (ev: El V) ⊢
  proj 2 2 (cons et (cons eu ev)) ≡ ev;
assert (T: Set) (U: Set) (V: Set) (et: El T) (eu: El U) (ev: El V) ⊢
  proj 1 2 (cons et (cons eu ev)) ≡ eu;
assert (T: Set) (U: Set) (V: Set) (et: El T) (eu: El U) (ev: El V) ⊢
  proj 0 2 (cons et (cons eu ev)) ≡ et;
