require open personoj.lhol personoj.pvs_cert personoj.tuple;
require personoj.extra.arity-tools as A;

// An inhabitant of [Restriction a b] is an evidence that [a] is a subtype of
// [b].
constant symbol Restriction: Set → Set → TYPE;
constant symbol Rest-refl (a: Set): Restriction a a;
constant symbol Rest-sub (a: Set) (p: El (a ~> prop)): Restriction (psub [a] p) a;
constant symbol Rest-trans (a b c: Set):
  Restriction a b → Restriction b c → Restriction a c;
constant symbol Rest-fun (d r0 r1: Set):
  Restriction r0 r1 → Restriction (d ~> r0) (d ~> r1);
constant symbol Rest-tuple [len: A.N]
													 (hd0: Set) (tl0: SVec len)
													 (hd1: Set) (tl1: SVec len):
	Restriction hd0 hd1 → Restriction (σ tl0) (σ tl1) →
	Restriction (σ (hd0 & tl0)) (σ (hd1 & tl1));

// Note that proof irrelevance shouldn't be needed because once the proof is
// given, the cast reduces

// [cast a b re x] casts element [x] from type [a] to type [b] given the
// evidence [re] that [a] is a restriction of [b].
symbol cast (a: Set) (b: Set) (_: Restriction a b) (_:El a): El b;
rule cast _ _ (Rest-refl _) $x ↪ $x;
rule cast _ _ (Rest-trans $a $b $c $prf-ab $prf-bc) $x ↪
  cast $b $c $prf-bc (cast $a $b $prf-ab $x);
rule cast _ _ (Rest-sub _ _) $x ↪ fst $x;
rule cast _ _ (Rest-fun _ $r0 $r1 $proof) $f ↪ λ x, cast $r0 $r1 $proof ($f x);
rule cast _ _ (Rest-tuple $h0 $t0 $h1 $t1 $proof-hd $proof-tl) $l ↪
  (cast $h0 $h1 $proof-hd (head $l)) ^ (cast (σ $t0) (σ $t1) $proof-tl (tail $l));

assert (a: Set) (x: El a) ⊢ cast a a (Rest-refl a) x ≡ x;
assert (a: Set) (p: El (a ~> prop)) (x: El (psub p)) ⊢
  cast (psub p) a (Rest-sub a p) x ≡ fst x;
assert (a: Set) (p: El (a ~> prop)) (q: El (psub p ~> prop)) (x: El (psub q)) ⊢
  cast (psub q) a (Rest-trans (psub q) (psub p) a
  														(Rest-sub _ q)
  														(Rest-sub _ p))
  		 x ≡ fst (fst x);

/* HACK as long as the exporter does not introduce a proof of subtyping when
   variables are declared as subtypes of others, this symbol is required */
symbol cast-proof (a: Set) (b: Set): Restriction a b;
/* Normally, a declaration of the form th [X: TYPE, Y: TYPE FROM X] foo: [Y -> Y] END
   should be translated as
   foo: Π (X: Set) (Y: Set) (h: Restriction X Y): El Y;
   so that each call to cast can instantiate the third argument with `h'.  But
   this requires work (to be done) on the exporter so that when `foo' is
   applied, it detects that there is subtyping involved, add the proof to the
   arguments. As long as such a mechanism isn't implemented, subtyping proofs
   are fulfilled with `cast-proof'. Leaving metavariables isn't correct either,
   unifications may happen between terms that contain such proofs. */
