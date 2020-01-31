require open encodings.cf

definition bool ≔ Prop
definition false ≔ @prod Type Prop uProp (λ x : Term uProp, x)
definition true ≔ Term false ⇒ Term false

definition bnot (P: Term uProp) ≔ @prod Prop Prop P (λ x, false)
