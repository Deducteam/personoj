require open encodings.cert_f

definition bool ≔ uProp
definition false: Term bool ≔ @prod Type Prop uProp (λ x, x)
definition true: Term bool ≔ @prod Prop Prop false (λ _, false)

definition imp (P Q: Term uProp): Term bool ≔ @prod Prop Prop P (λ_, Q)
definition forall {eT: Term uType} (P: Term eT ⇒ Term bool): Term bool ≔
  @prod _ _ eT P

definition bnot (P: Term uProp): Term bool ≔ @prod Prop Prop P (λ _, false)
set prefix 5 "¬" ≔ bnot

definition band (P Q: Term uProp) ≔ bnot (imp P (bnot Q))
set infix 6 "∧" ≔ band
definition bor (P Q: Term uProp) ≔ imp (bnot P) Q
set infix 5 "∨" ≔ bor

definition biff (P Q: Term bool) ≔ (imp P Q) ∧ (imp Q P)
set infix 7 "⇔" ≔ biff

definition when (P Q: Term uProp) ≔ imp Q P
// FIXME explicitness?

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "imp" ≔ imp
set builtin "and" ≔ band
set builtin "or" ≔ bor
set builtin "not" ≔ bnot
