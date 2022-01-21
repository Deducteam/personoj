// Equality
require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical;

// Equality is uncurried because PVS' equality is uncurried. Using
// a curried equality may raise unification failures. It is the case in
// KernelDefinition.PreservesEq_is_preserving, where there is an equality (=
// (M ^^ (= [Y]))), where M is of type (El (Y && Y) -> Prop), thus (= [Y])
// must be of type (El (Y && Y) -> Prop).

constant symbol = [a: Set] (_: El (σ (a && a))): Prop;
constant symbol eqind [a: Set] (p: El a → Prop) (x y: El a): Prf (p x) → Prf (@= a (x ^^ y)) → Prf (p y);
constant symbol refl [a: Set] (x: El a): Prf (@= a (x ^^ x));

opaque symbol eq_psub [a: Set] (t u: El a) (p: El a → Prop)
                      (ht: Prf (p t)) (hu: Prf (p u))
                      (e: Prf (@= a (t ^^ u))):
  Prf (@= (@psub a p) ((@pair a p t ht) ^^ (@pair a p u hu))) ≔
  @eqind a
  (λ x: El a, p x ⇒ (λ h: Prf (p x), @= (@psub a p) ((@pair a p t ht) ^^ (@pair a p x h)))) t u
  (λ ht': Prf (p t), @refl (@psub a p) (@pair a p t ht')) e hu;

symbol != [a: Set] (v: El (σ (a && a))) ≔ ¬ (@= a v);

// Equality operates on the maximal supertype. It allows to profit
// from predicate subtyping for free in the propositional equality.
//rule @eq (@psub $t $p) ($x ^ $y ^ ^nil)
//   ↪ @eq $t ((@fst $t $p $x) ^ (@fst $t $p $y) ^ ^nil);
