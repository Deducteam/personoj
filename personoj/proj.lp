require open personoj.lhol personoj.nat personoj.tuple personoj.sum;

/// [Proj n l t] returns the element at index [n] of tuple [t] of length [l].
symbol Proj : Nat → Nat → Π (a: Set), El a → TYPE;
rule Proj 0 0 $a _ ↪ El $a with
    Proj 0 (succ _) (Σ $a _) _ ↪ El $a with
    Proj 0 (succ _) (σ $a _) _ ↪ El $a with
    Proj (succ $n) (succ $l) (Σ _ $u) (consd $x $y) ↪ Proj $n $l ($u $x) $y with
    Proj (succ $n) (succ $l) (σ _ $u) (cons _ $y) ↪ Proj $n $l $u $y;

assert (T: Set) (U: Set) (x: El T) (y: El U) ⊢ Proj 1 1 (σ T U) (cons x y) ≡ El U;
assert (T: Set) (U: El T → Set) (x: El T) (y: El (U x)) ⊢ Proj 1 1 (Σ T U) (consd x y) ≡ El (U x);
assert (T: Set) (U: Set) (x: El T) (y: El U) ⊢ Proj 0 1 (σ T U) (cons x y) ≡ El T;
assert (T: Set) (U: El T → Set) (x: El T) (y: El (U x)) ⊢ Proj 0 1 (Σ T U) (consd x y) ≡ El T;
assert (T: Set) (U: Set) (V: Set) (x1: El T) (x2: El U) (x3: El V) ⊢
    Proj 2 2 (σ T (σ U V)) (cons x1 (cons x2 x3)) ≡ El V;

/// [proj{T} n l e] returns the element at index [n] of list (made of pairs) [e]
/// of type [T] of length [l], where [length (σ _ B) = 1 + length B] and [length
/// t = 0] if [t] is not a pair.
symbol proj {a: Set} (n: Nat) (l: Nat) (x: El a): Proj n l a x;
rule @proj (σ _ _) 0 (succ _) (cons $x _) ↪ $x with
    @proj (Σ _ _) 0 (succ _) (consd $x _) ↪ $x with
    @proj (σ _ _) 1 1 (cons _ $y) ↪ $y with
    @proj (Σ _ _) 1 1 (consd _ $y) ↪ $y with
    @proj (σ _ $b) (succ $n) (succ (succ $m)) (cons _ $y) ↪
     @proj $b $n (succ $m) $y with
    @proj (Σ _ $b) (succ $n) (succ (succ $m)) (consd $x $y) ↪
     @proj ($b $x) $n (succ $m) $y ;

assert (T: Set) (e: El T) (e': El T) ⊢ proj {σ T T} 1 1 (cons e e') ≡ e';
assert (T: Set) (U: El T → Set) (e: El T) (e': El (U e)) ⊢ proj {Σ T U} 1 1 (consd e e') ≡ e';
assert (T: Set) (e: El T) (e': El T) ⊢ proj {σ T T} 0 1 (cons e e') ≡ e;
assert (T: Set) (U: El T → Set) (e: El T) (e': El (U e)) ⊢ proj {Σ T U} 0 1 (consd e e') ≡ e;
assert (T: Set) (U: Set) (V: Set) (et: El T) (eu: El U) (ev: El V) ⊢
  proj 2 2 (cons et (cons eu ev)) ≡ ev;
assert (T: Set) (U: Set) (V: Set) (et: El T) (eu: El U) (ev: El V) ⊢
  proj 1 2 (cons et (cons eu ev)) ≡ eu;
assert (T: Set) (U: Set) (V: Set) (et: El T) (eu: El U) (ev: El V) ⊢
  proj 0 2 (cons et (cons eu ev)) ≡ et;
