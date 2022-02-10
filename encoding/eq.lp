// Equality
require open personoj.lhol personoj.pvs_cert personoj.logical;
require personoj.telescope as TL;

// Equality is uncurried because PVS' equality is uncurried. Using
// a curried equality may raise unification failures. It is the case in
// KernelDefinition.PreservesEq_is_preserving, where there is an equality (=
// (double M (= [Y]))), where M is of type (El (double! Y Y) -> Prop), thus (= [Y])
// must be of type (El (double! Y Y) -> Prop).

constant symbol = [a: Set] (_: El (TL.code (TL.double! a a))): Prop;
constant symbol eqind [a: Set] (p: El a → Prop) (x y: El a):
  Prf (p x) → Prf (@= a (TL.double x y)) → Prf (p y);
constant symbol refl [a: Set] (x: El a): Prf (@= a (TL.double x x));

opaque symbol eq_psub [a: Set] (t u: El a) (p: El a → Prop)
                      (ht: Prf (p t)) (hu: Prf (p u))
                      (e: Prf (@= a (TL.double t u))):
  Prf (@= (@psub a p) (TL.double (@pair a p t ht) (@pair a p u hu))) ≔
  @eqind a
  (λ x: El a, p x ⇒ (λ h: Prf (p x), @= (@psub a p) (TL.double (@pair a p t ht) (@pair a p x h)))) t u
  (λ ht': Prf (p t), @refl (@psub a p) (@pair a p t ht')) e hu;

symbol != [a: Set] (v: El (TL.code (TL.double! a a))) ≔ ¬ (@= a v);
