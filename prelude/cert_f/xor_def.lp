require open encodings.cert_f prelude.cert_f.booleans prelude.cert_f.equalities
prelude.cert_f.notequal prelude.cert_f.if_def

// FIXME explicitness required
definition xor (a b: Univ Prop) ≔ @neq uProp a b

type ∀ (a b: Univ Prop), Term (xor a b)

type λ (a: Term uProp), @if uProp a a a
type ∀ a : Term uProp, Term (@if uProp a a a)

// FIXME explicitness required
theorem xor_def: ∀ (a b: Univ Prop),
  Term (@eq uProp (xor a b) (@if uProp a (bnot b) b))
proof
assume a b
simpl
admit
