require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert

definition false ≔ forall {bool} (λx, x)
definition true ≔ impd {false} (λ_, false)

definition imp P Q ≔ impd {P} (λ_, Q)

definition bnot P ≔ impd {P} (λ_, false)
definition band P Q ≔ bnot (imp P (bnot Q))
definition bor P Q ≔ imp (bnot P) Q
set prefix 5 "¬" ≔ bnot
set infix 6 "∧" ≔ band
set infix 5 "∨" ≔ bor

definition biff P Q ≔ (imp P Q) ∧ (imp Q P)
set infix 7 "⇔" ≔ biff

definition when P Q ≔ imp Q P

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "imp" ≔ imp
set builtin "not" ≔ bnot
set builtin "and" ≔ band
set builtin "or"  ≔ bor

// Lazy reduction rules
//rule bor true → λ_, true
//rule bor false → λx, x
//rule band true → λx, x
//rule band false → λ_, false
