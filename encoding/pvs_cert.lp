// Predicate subtyping
require open personoj.lhol;

// [psub [a] p] is the encoding of {x: a | p}
constant symbol psub [x: Set]: (El x → Prop) → Set;
protected constant symbol pair': Π(a: Set) (p: El a → Prop), El a → El (@psub a p);
symbol fst: Π[a: Set] [p: El a → Prop], El (@psub a p) → El a;

symbol snd [a: Set] [p: El a → Prop] (m: El (@psub a p)): Prf (p (@fst a p m));

// Proof irrelevance
redopaque
symbol pair [a: Set] [p: El a → Prop] (m: El a) (_: Prf (p m))
     ≔ pair' a p m;

rule @fst _ _ (pair' _ _ $M) ↪ $M;
