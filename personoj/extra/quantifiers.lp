// Extra quantifiers
require open personoj.lhol personoj.logical personoj.nat;
require personoj.extra.arity-tools as A;

// A variadic forall quantification [∀* {n} tys ex] quantifies over
// [n] variables of types [tys] using binder [ex].
symbol ∀* {n: A.N} (a*: A.Vec n) (b: El (A.vec->arr {n} a* prop)): Prop;
rule ∀* {A.s $n} (A.cons $d $tl) $b ↪
     ∀ {$d} (λ x: El $d, ∀* $tl ($b x))
with ∀* {A.z} _ $e ↪ $e;

assert (b: El prop) ⊢ ∀* {A.z} A.nil b ≡ b;
assert (e: El (prop ~> prop)) ⊢ ∀* (A.vec A.one prop) e ≡ ∀ {prop} (λ x, e x);
assert (e: El (prop ~> prop ~> prop)) ⊢
∀* (A.vec A.two prop prop) e ≡ ∀ (λ x, ∀ (λ y, e x y));

// Same as ∀* but for existential.
symbol ∃* {n: A.N} (a*: A.Vec n) (b: El (A.vec->arr {n} a* prop)): Prop;
rule ∃* {A.s $n} (A.cons $d $tl) $b ↪ ∃ {$d} (λ x: El $d, ∃* $tl ($b x))
with ∃* {A.z} _ $e ↪ $e;

assert (b: El prop) ⊢ ∃* {A.z} A.nil b ≡ b;
assert (e: El (prop ~> prop)) ⊢ ∃* (A.cons prop A.nil) (λ x, e x) ≡ ∃ {prop} (λ x, e x);
