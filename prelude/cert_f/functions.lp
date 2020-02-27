require open encodings.cert_f
prelude.cert_f.logic


//
// functions [D, R: TYPE]
//

definition {|injective?|} {eD eR: Term uType} (f g: Term eD ⇒ Term eR) ≔
  ∀ x1 x2, Term (eq (f x1) (f x2)) ⇒ Term (eq x1 x2)
