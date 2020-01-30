require open encodings.cf

definition bool ≔ Prop
definition false ≔ @prod Type Prop uProp (λ x : @Term Type uProp, x)
definition true ≔ @Term Prop false ⇒ @Term Prop false

definition bnot (P: @Term Type uProp) ≔ @Term Prop P ⇒ @Term Prop false
// definition band (P Q: @Term Type uProp) ≔ @Term Prop P ⇒ (bnot (@Term Prop Q))
