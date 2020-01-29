require open pvs_cert

definition bool ≔ Prop

symbol r : Rule Type Prop Prop
symbol a : Axiom Prop Type

definition uProp ≔ univ Prop Type I

compute Term Type uProp
type uProp

definition false ≔ prod Type Prop Prop uProp r (λ x : Term Type uProp, x)
definition true ≔ Term Prop false ⇒ Term Prop false
