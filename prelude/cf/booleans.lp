require open encodings.cf

definition bool ≔ Prop
definition false ≔ @prod Type Prop uProp (λ x : @Term Type uProp, x)
definition true ≔ @Term Prop false ⇒ @Term Prop false

definition bnot (P: @Term Type uProp) ≔ @prod Prop Prop P (λ x, false)
