// FIXME: implicit arguments must be avoided in the definition of coercions
// because they are not type checked properly.
require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical personoj.sum;

coercion "set-pred" (t: Set) ⊢ (λ _: El t, true) : El t → El prop on 1;
coercion "pred-set" (t: Set) (p: El (t ~> prop)) ⊢ @psub t p : Set on 2;

coercion "psub-fst"
  (t: Set) (p: El t → Prop) (m: El (@psub t p)) ⊢ @fst t p m : El t
  on 3;

coercion "psub-pair"
  (t: Set) (p: El t → Prop) (m: El t) (π: Prf (p m)) ⊢ @pair t p m π : El (@psub t p)
  on 3;

coercion "psub-fst-tr"
  (t: Set) (u: Set) (p: El t → Prop) (m: El (@psub t p)) ⊢ $c[@fst t p m] : El u
  on 4
  with c : El t → El u;

coercion "psub-pair-tr"
  (t: Set) (u: Set) (p: El u → Prop) (m: El t) (π: Prf (p _)) ⊢ @pair u p ($c[m]) π : El (@psub u p)
  on 4
with c : El t → El u;

// Coercion:
// coercion "tup-nil"
//   (a b a' b': Set) (t: El (σ {A.z} (A.vec A.two a b))) ⊢
//   @double a' b' $c[@car A.z (A.vec A.two a b) t] $d[@last A.z (A.vec A.two a b) t]:
//   El (σ (A.vec A.two a' b'))
//  on 5
//  with c: El a → El a'
//  with d: El b → El b';

require personoj.extra.arity-tools as A;
// The following coercion is redundant with 'tup-cons'
coercion "2-uple"
  (a a' b b': Set) (arg: El (σ {A.two} (a && b))) ⊢
  @^^ a' b' $c[@head A.two (a && b) arg] $d[@head A.one (@& A.z b &nil) (@tail A.one (a && b) arg)]:
  El (σ {A.two} (a' && b'))
  on 5
  with c: El a → El a'
  with d: El b → El b';
coercion "tup-cons"
  (a a': Set) (n: A.N) (v v': SVec n) (arg: El (@σ (A.s n) (@& n a v))) ⊢
  @^ n a' v' $c[@head (A.s n) (@& n a v) arg] $d[@tail n (@& n a v) arg]:
  El (@σ (A.s n) (@& n a' v'))
 on 6
 with c: El a → El a'
 with d: El (@σ n v) → El (@σ n v');

// The following rule does not typecheck because of the last pre requisite
// coercion "dtuple"
//  (a0: Set) (b0: El a0 → Set) (a1: Set) (b1: El a1 → Set) (t: El (Σ a0 b0)) ⊢
//     @consd a0 b0 $c[@card a0 b0 t] $d[@cdrd a0 b0 t]: El (Σ a1 b1)
//  on 5
// with c : El a0 → El a1
// with d : El (b0 (@card a0 b0 t)) → El (b1 $c[@card a0 b0 t]);
