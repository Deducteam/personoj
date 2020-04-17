require open personoj.encodings.cert_f

definition bool ≔ uProp
definition false ≔ prod bool (λ x, x)
definition true ≔ prod false (λ _, false)

definition imp (P Q: Term uProp) ≔ prod P (λ_, Q)
definition forall {T: Term uType} (P: Term T → Term bool) ≔ prod T P

definition bnot (P: Term uProp) ≔ prod P (λ _, false)
set prefix 5 "¬" ≔ bnot

definition band P Q ≔ bnot (imp P (bnot Q))
set infix 6 "∧" ≔ band
definition bor P Q ≔ imp (bnot P) Q
set infix 5 "∨" ≔ bor

definition biff P Q ≔ (imp P Q) ∧ (imp Q P)
set infix 7 "⇔" ≔ biff

definition when P Q ≔ imp Q P

set builtin "bot" ≔ false
set builtin "top" ≔ true
set builtin "imp" ≔ imp
set builtin "and" ≔ band
set builtin "or"  ≔ bor
set builtin "not" ≔ bnot

// Defined here because ⊑ needs it.
symbol eq {T: Term uType}: Term T → Term T → Term bool
set infix 5 "=" ≔ eq
// set builtin "eq" ≔ eq
