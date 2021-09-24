require open personoj.lhol;
require open personoj.extra.tuple;
require personoj.extra.arity-tools as A;

// Coercion:
coercion "double"
  (a b a' b': Set) (t: El (σ {A.z} (A.vec A.two a b))) ⊢
  @double a' b' $c[@car A.z (A.vec A.two a b) t] $d[@last A.z (A.vec A.two a b) t]:
  El (σ (A.vec A.two a' b'))
 on 5
 with c: El a → El a'
 with d: El b → El b';

coercion "tcons"
  (a a': Set) (n: A.N) (v v': A.Vec (+2 n)) (t: El (@σ (A.s n) (@A.cons (+2 n) a v))) ⊢
  @cons n a' v' $c[@car (A.s n) a t] $d[@cdr n v t]:
  El (@σ (A.s n) (@A.cons (+2 n) a' v'))
 on 6
 with c: El a → El a'
 with d: El (@σ (+2 n) v) → El (@σ (+2 n) v');

// TODO: some tests
