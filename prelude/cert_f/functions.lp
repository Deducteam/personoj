require open encodings.cert_f
adlib.cert_f.booleans
prelude.cert_f.logic
require adlib.cert_f.subtype as S

definition arrow (D R: Term uType) ≔ prod D (λ_, R)

//
// functions [D, R: TYPE]
//

definition {|injective?|} {D: Term uType} {R:Term uType} (f: Term (arrow D R))
  ≔ forall (λx1, forall (λx2, imp (eq (f x1) (f x2)) (eq x1 x2)))

definition {|surjective?|} {D: Term uType} {R: Term uType} (f: Term (arrow D R))
  ≔ forall (λy, ∃ (λx, eq (f x) y))

definition {|bijective?|} {D: Term uType} {R: Term uType} (f: Term (arrow D R))
  ≔ ({|injective?|} f) ∧ ({|surjective?|} f)

theorem bij_is_inj (D R: Term uType):
  Term (Psub {arrow D R} {|bijective?|} ⊑ Psub {arrow D R} {|injective?|})
proof
admit

theorem bij_is_surj (D R: Term uType):
  Term (Psub {arrow D R} {|bijective?|} ⊑ Psub {arrow D R} {|surjective?|})
proof
admit

symbol domain {D: Term uType} {R: Term uType} (f: Term (arrow D R)): Term uType
rule domain {&D} {_} _ → &D

//
// restrict[T: TYPE, S: TYPE FROM T, R: TYPE]
//
symbol restrict {T: Term uType} (S: Term uType) {R: Term uType}
  (f: Term (arrow T R)) (_: Term (S ⊑ T)) (s: Term S)
  : Term R
rule restrict {&T} _ {_} &f &pr &s → &f (↑ &T &pr &s)

theorem injective_restrict {T} {R} (f: Term (arrow T R))
  (S: Term uType) (pr: Term (S ⊑ T))
  : Term ({|injective?|} f) ⇒ Term ({|injective?|} (restrict S f pr))
proof
admit

//
// restrict_props[T: TYPE, R: TYPE]
//

theorem restrict_full {T: Term uType} {R: Term uType} (f: Term (arrow T R))
  : Term (eq {arrow T R} (restrict {T} T {R} f (S.refl T)) f)
proof
admit

//
// extend[T: TYPE, S: TYPE FROM T, R: TYPE, d: R]
//

definition extend {T: Term uType}
  (s_pred: Term (pred T))
  {R: Term uType} (d: Term R)
  (f: Term (arrow (Psub s_pred) R))
  (t: Term T)
  (pr: Term (s_pred t))
  ≔ if (s_pred t) (f (↓ s_pred t pr)) d
