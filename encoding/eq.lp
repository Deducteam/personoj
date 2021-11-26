// Equality
require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical;

symbol = {a: Set} (_ _: El a): Prop;
symbol eqind {a: Set} (p: El a → Prop) (x y: El a): Prf (p x) → Prf (@= a x y) → Prf (p y);

symbol refl {a: Set} (x: El a): Prf (@= a x x); 

opaque symbol eq_psub (a: Set) (t u: El a) (p: El a → Prop)
											(ht: Prf (p t)) (hu: Prf (p u))
											(e: Prf (@= a t u)):
	Prf (@= (@psub a p) (@pair a p t ht) (@pair a p u hu)) ≔
  @eqind a
	(λ x: El a, (p x) ⇒ (λ h: Prf (p x), @= (@psub a p) (@pair a p t ht) (@pair a p x h))) t u
	(λ ht': Prf (p t), @refl (@psub a p) (@pair a p t ht')) e hu;


// Equality operates on the maximal supertype. It allows to profit
// from predicate subtyping for free in the propositional equality.
//rule @eq (@psub $t $p) ($x ^ $y ^ ^nil)
//   ↪ @eq $t ((@fst $t $p $x) ^ (@fst $t $p $y) ^ ^nil);

symbol != {a: Set} (x y: El a) ≔ ¬ (@= a x y);
