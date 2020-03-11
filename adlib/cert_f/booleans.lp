require open encodings.cert_f

definition bool ≔ uProp
definition false ≔ @prod Type Prop bool (λ x, x)
definition true ≔ @prod Prop Prop false (λ _, false)

definition imp (P Q: Term uProp) ≔ @prod Prop Prop P (λ_, Q)
definition forall {T: Term uType} (P: Term T ⇒ Term bool) ≔ @prod _ _ T P

definition bnot (P: Term uProp) ≔ @prod Prop Prop P (λ _, false)
set prefix 5 "¬" ≔ bnot

definition band (P Q: Term uProp) ≔ bnot (imp P (bnot Q))
set infix 6 "∧" ≔ band
definition bor (P Q: Term uProp) ≔ imp (bnot P) Q
set infix 5 "∨" ≔ bor

definition biff (P Q: Term bool) ≔ (imp P Q) ∧ (imp Q P)
set infix 7 "⇔" ≔ biff

definition when (P Q: Term uProp) ≔ imp Q P

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "imp" ≔ imp
set builtin "and" ≔ band
set builtin "or"  ≔ bor
set builtin "not" ≔ bnot
