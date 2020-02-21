require open encodings.cert_f

definition bool ≔ uProp
definition false ≔ @prod Type Prop uProp (λ x, x)
definition true ≔ @prod Prop Prop false (λ _, false)

definition bnot (P: Term uProp) ≔ @prod Prop Prop P (λ _, false)
