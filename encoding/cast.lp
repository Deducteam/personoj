require open personoj.lhol personoj.pvs_cert;

// An inhabitant of [Restriction a b] is an evidench that [a] is a subtype of
// [b].
constant symbol Restriction: Set → Set → TYPE;
constant symbol Rest-refl (a: Set): Restriction a a;
constant symbol Rest-sub (a: Set) (p: El (a ~> prop)): Restriction (psub [a] p) a;
constant symbol Rest-trans (a b c: Set):
  Restriction a b → Restriction b c → Restriction a c;

// Note that proof irrelevance shouldn't be needed because there is only one
// proof possible which removes each subtype successively.

// [cast a b re x] casts element [x] from type [a] to type [b] given the
// evidence [re] that [a] is a restriction of [b].
symbol cast (a: Set) (b: Set) (_: Restriction a b) (_:El a): El b;
rule cast _ _ (Rest-refl _) $x ↪ $x;
rule cast _ _ (Rest-sub _ _) $x ↪ fst $x;
