require open encodings.cert_f
prelude.cert_f.booleans

// Allows to make case disjunction in proofs,
// [refine disjunction P ?Cfalse ?Ctrue]
// to create two new subgoals
symbol disjunction (P: Term bool ⇒ Term bool):
  Term (P false) ⇒ Term (P true) ⇒ ∀ x, Term (P x)
