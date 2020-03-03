require open encodings.cert_f
adlib.cert_f.booleans
prelude.cert_f.logic


//
// functions [D, R: TYPE]
//

definition {|injective?|} {D R: Term uType} (f: Term D ⇒ Term R):
  Term bool ≔
  forall (λx1, forall (λx2, imp (eq (f x1) (f x2)) (eq x1 x2)))

definition {|surjective?|} {D R: Term uType} (f: Term D ⇒ Term R): Term bool ≔
  forall (λy, ∃ (λx, eq (f x) y))

// type λD R (f: Term D ⇒ Term R),@{|injective?|} D R f

definition {|bijective?|} {D R: Term uType} (f: Term D ⇒ Term R): Term bool ≔
  (@{|injective?|} D R f) ∧ (@{|surjective?|} D R f)

// FIXME, the following is accepted (see 'implicits' arguments on injective)
// definition {|bijective?|} {D R: Term uType} (f: Term D ⇒ Term R): Term bool ≔
//  ({|injective?|} R f) ∧ (@{|surjective?|} D R f)
