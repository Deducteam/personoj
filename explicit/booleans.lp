require open pvs_cert explicit.pts_bindings

definition bool ≔ Prop

definition false ≔ prod Type Prop Prop uProp r (λ x : Term Type uProp, x)
definition true ≔ Term Prop false ⇒ Term Prop false

definition bnot (P : Term Type uProp) ≔ Term Prop P ⇒ Term Prop false
