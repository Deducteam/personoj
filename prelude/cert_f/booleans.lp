require open encodings.cert_f

definition bool ≔ uProp
definition false ≔ @prod Type Prop uProp (λ x, x)
definition true ≔ @prod Prop Prop false (λ _, false)

definition imp (P Q: Term uProp) ≔ @prod Prop Prop P (λ_, Q)

definition bnot (P: Term uProp) ≔ @prod Prop Prop P (λ _, false)
set prefix 5 "¬" ≔ bnot
definition band (P Q: Term uProp) ≔ bnot (imp P (bnot Q))
set infix 6 "∧" ≔ band
definition bor (P Q: Term uProp) ≔ imp (bnot P) Q
set infix 5 "∨" ≔ bor
