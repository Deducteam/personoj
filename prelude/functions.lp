require open personoj.encodings.cert_f
  personoj.adlib.bootstrap
  personoj.prelude.logic
require personoj.adlib.subtype as S

//
// functions [D, R: TYPE]
//

// symbol
// extensionality_postulate (D R: Term uType) (f g: Term (D ~> R))
// : Term (biff (forall (λx: Term D, f x = g ) (f = g)))

definition {|injective?|} {D} {R} (f: Term (D ~> R)) ≔
  forall (λx1, forall (λx2, imp (f x1 = f x2) (x1 = x2)))

definition {|surjective?|} {D: Term uType} {R: Term uType} (f: Term (D ~> R)) ≔
  forall (λy, ∃ (λx, (f x) = y))

definition {|bijective?|} {D: Term uType} {R: Term uType} (f: Term (D ~> R)) ≔
  ({|injective?|} f) ∧ ({|surjective?|} f)

theorem bij_is_inj {D: Term uType} {R: Term uType}:
  Term (Psub {D ~> R} {|bijective?|} ⊑ Psub {D ~> R} {|injective?|})
proof
admit

theorem bij_is_surj {D: Term uType} {R: Term uType}:
  Term (Psub {D ~> R} {|bijective?|} ⊑ Psub {D ~> R} {|surjective?|})
proof
admit

symbol domain {D: Term uType} {R: Term uType} (f: Term (D ~> R)): Term uType
rule domain {&D} {_} _ → &D

//
// restrict[T: TYPE, S: TYPE FROM T, R: TYPE]
//
symbol restrict {T: Term uType} (S: Term uType) {R: Term uType}
                (f: Term (T ~> R)) (_: Term (S ⊑ T)) (s: Term S):
  Term R
rule restrict {&T} _ {_} &f &pr &s → &f (↑ &T &pr &s)

theorem injective_restrict {T} S {R} (f: Term (T ~> R)) (pr: Term (S ⊑ T)):
  Term ({|injective?|} f) ⇒ Term ({|injective?|} (restrict S f pr))
proof
admit

//
// restrict_props[T: TYPE, R: TYPE]
//

theorem restrict_full {T: Term uType} {R: Term uType} (f: Term (T ~> R)):
  Term (eq {T ~> R} (restrict {T} T {R} f (S.refl T)) f)
proof
admit

//
// extend[T: TYPE, S: TYPE FROM T, R: TYPE, d: R]
//

definition extend {T: Term uType}
  (s_pred: Term (pred T)) {R: Term uType} (d: Term R)
  (f: Term (Psub s_pred ~> R)) (t: Term T) (pr: Term (s_pred t)) ≔
  if (s_pred t) (f (↓ s_pred t pr)) d
