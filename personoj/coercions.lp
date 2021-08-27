require open personoj.lhol personoj.pvs_cert personoj.logical personoj.tuple;

coercion "set-pred" (t: Set) ⊢ (λ _: El t, true) : El t → El prop on 1;
//coercion "set-pred" λ (t: Set) (_: El t), true : Π t : Set, (El (t ~> prop)) on 1;
coercion "pred-set" (t: Set) (p: El (t ~> prop)) ⊢ @psub t p : Set on 2;
//coercion "pred-set" @psub : Π (t: Set) (p: El (t ~> prop)), Set on 2;

// coercion "psub-fst"
//   λ (t: Set) (p: El t → Prop) (m: El (@psub t p)), @fst t p m :
//   Π (t: Set) (p: El (t ~> prop)) (m : El (@psub t p)), El t
// on 3;
coercion "psub-fst"
  (t: Set) (p: El t → Prop) (m: El (@psub t p)) ⊢ @fst t p m : El t
  on 3;
// coercion "psub-pair"
//   λ (t: Set) (p: El t → Prop) (m: El t) (π: Prf (p m)), @pair t p m π :
//   Π (t: Set) (p: El (t ~> prop)) (m: El t) (_: Prf (p m) ), El (@psub t p)
// on 3;
coercion "psub-pair"
  (t: Set) (p: El t → Prop) (m: El t) (π: Prf (p m)) ⊢ @pair t p m π : El (@psub t p)
  on 3;
// coercion "psub-fst-tr"
//   λ (t: Set) (u: Set) (p: El t → Prop) (m: El (@psub t p)), $c[@fst t p m] :
//   Π (t: Set) (u: Set) (p: El t → Prop) (m: El (@psub t p)), El u
//   on 4
// with c : El t → El u;
coercion "psub-fst-tr"
  (t: Set) (u: Set) (p: El t → Prop) (m: El (@psub t p)) ⊢ $c[@fst t p m] : El u
  on 4
  with c : El t → El u;
// coercion "psub-pair-tr"
//   λ (t: Set) (u: Set) (p: El u → Prop) (m: El t) (π: Prf (p _)), @pair u p ($c[m]) π :
//   Π (t: Set) (u: Set) (p: El u → Prop) (m: El t) (_: Prf (p _)), El (@psub u p)
//   on 4
// with c : El t → El u;
coercion "psub-pair-tr"
  (t: Set) (u: Set) (p: El u → Prop) (m: El t) (π: Prf (p _)) ⊢ @pair u p ($c[m]) π : El (@psub u p)
  on 4
with c : El t → El u;

// coercion "tuple"
//   λ (a0 b0 a1 b1: Set) (t: El (σ a0 b0)),
//     cons {a1} {b1} $c[car {a0} {b0} t] $d[cdr {a0} {b0} t]:
//   Π (a0 b0 a1 b1: Set) (_: El (σ a0 b0)), El (σ a1 b1)
//   on 5
// with c : El a0 → El a1
// with d : El b0 → El b1;

coercion "tuple"
  (a0 b0 a1 b1: Set) (t: El (σ a0 b0)) ⊢
    @cons a1 b1 $c[@car a0 b0 t] $d[@cdr a0 b0 t]: El (σ a1 b1)
  on 5
with c : El a0 → El a1
with d : El b0 → El b1;
