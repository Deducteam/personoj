// Extra quantifiers
require open personoj.lhol personoj.logical personoj.nat;
require open personoj.extra.arity-tools;
require personoj.alt.SetVec as SVec;

// A variadic forall quantification [∀* [n] tys ex] quantifies over
// [n] variables of types [tys] using binder [ex].
symbol ∀* [n: N] (a*: SVec.T n) (b: El (SVec.mkarr [n] a* prop)): Prop;
rule ∀* [s $n] (SVec.cons $d $tl) $b ↪
     ∀ [$d] (λ x: El $d, ∀* $tl ($b x))
with ∀* [z] _ $e ↪ $e;

assert (b: El prop) ⊢ ∀* [z] SVec.nil b ≡ b;
assert (e: El (prop ~> prop)) ⊢ ∀* (SVec.vec one prop) e ≡ ∀ [prop] (λ x, e x);
assert (e: El (prop ~> prop ~> prop)) ⊢
∀* (SVec.vec two prop prop) e ≡ ∀ (λ x, ∀ (λ y, e x y));

// Same as ∀* but for existential.
symbol ∃* [n: N] (a*: SVec.T n) (b: El (SVec.mkarr [n] a* prop)): Prop;
rule ∃* (SVec.cons $d $tl) $b ↪ ∃ [$d] (λ x: El $d, ∃* $tl ($b x))
with ∃* SVec.nil $e ↪ $e;

assert (b: El prop) ⊢ ∃* [z] SVec.nil b ≡ b;
assert (e: El (prop ~> prop)) ⊢ ∃* (SVec.cons prop SVec.nil) (λ x, e x) ≡ ∃ [prop] (λ x, e x);
