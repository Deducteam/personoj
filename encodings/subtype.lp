require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

symbol subtype : Set ⇒ Set ⇒ Bool
set infix left 6 "⊑" ≔ subtype

symbol S_Refl (a: Set): ε (a ⊑ a)
symbol S_Trans (s t u: Set): ε (s ⊑ t) ⇒ ε (t ⊑ u) ⇒ ε (s ⊑ u)

symbol S_Restr {a: Set} (p: η a ⇒ Bool): ε (psub p ⊑ a)

symbol S_Arr (t u1 u2: Set): ε (u1 ⊑ u2) ⇒ ε ((t ~> u1) ⊑ (t ~> u2))
symbol S_Darr (d: Set) (r1: η d ⇒ Set) (r2: η d ⇒ Set)
: ε (forall (λx, (r1 x) ⊑ (r2 x))) ⇒ ε ((arrd r1) ⊑ (arrd r2))

symbol upcast {A: Set} {B: Set}: ε (A ⊑ B) ⇒ η A ⇒ η B

symbol root: Set ⇒ Set
rule root (psub {&T} _) → root &T


constant symbol SortList: TYPE
constant symbol SortCons: Set ⇒ SortList ⇒ SortList
constant symbol SortNil: SortList


symbol path': Set ⇒ SortList ⇒ SortList
rule path' (psub {&T} _) &L → path' &T (SortCons &T &L)
// [path T] Returns the path from T to the top type
definition path l ≔ path' l SortNil

// For each top type, e.g. numfield, add
// rule path' numfield &L → &L
constant symbol SortBool: TYPE
constant symbol STrue: SortBool
constant symbol SFalse: SortBool
symbol SIf: SortBool ⇒ SortBool ⇒ SortBool ⇒ SortBool
rule SIf STrue &t _ → &t
rule SIf SFalse _ &f → &f

// Equality on sorts
symbol SortEq: Set ⇒ Set ⇒ SortBool

// Membership on list of types
symbol mem: Set ⇒ SortList ⇒ SortBool
rule mem &X (SortCons &Y &L) → SIf (SortEq &X &Y) STrue (mem &X &L)
rule mem _ SortNil           → SFalse

symbol if_type: SortBool ⇒ Set ⇒ Set ⇒ Set
rule if_type STrue &t _ → &t
rule if_type SFalse _ &f → &f
// [supertype t u] returns the least common supertype of [t] and [u]
symbol supertype: Set ⇒ Set ⇒ Set
rule supertype &t (psub {&u} _) →
                  let x ≔ path &t in
                  if_type (mem &u x) &u (supertype &t &u)
//symbol Subset
