require open pvs_core

set builtin "T" ≔ eta
set builtin "P" ≔ eps

// booleans
definition bool ≔ Prop

// Minimal definitions, only the implication is primitive
definition false ≔ all bool (λ p : η bool, p)
definition true ≔ imp false false

definition bnot (P: η bool) ≔ imp P false
set prefix 5 "¬" ≔ bnot
definition band (P Q: η bool) ≔ ¬ (imp P (¬ Q))
definition bor (P Q: η bool) ≔ (imp (¬ P) Q)
set infix 6 "∧" ≔ band
set infix 5 "∨" ≔ bor
definition biff (P Q: η bool) ≔ (imp P Q) ∧ (imp Q P)
