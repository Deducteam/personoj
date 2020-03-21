require open personoj.encodings.cert_f
personoj.adlib.bootstrap

// Allows to make case disjunction in proofs,
// [refine disjunction P ?Cfalse ?Ctrue]
// to create two new subgoals
symbol disjunction (P: Term bool ⇒ Term bool):
  Term (P false) ⇒ Term (P true) ⇒ ∀ x, Term (P x)
