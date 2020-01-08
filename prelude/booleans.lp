require open pvs_core

set builtin "T" ≔ eta
set builtin "P" ≔ eps

// booleans
definition bool ≔ Prop
// symbol true : eta bool
// symbol false : eta bool
// symbol bnot : eta bool ⇒ eta bool
// symbol band : eta bool ⇒ eta bool ⇒ eta bool
// symbol bor : eta bool ⇒ eta bool ⇒ eta bool
// symbol bimpl : eta bool ⇒ eta bool ⇒ eta bool
// symbol biff : eta bool ⇒ eta bool ⇒ eta bool

// Minimal definitions, only the implication is primitive
definition false ≔ all bool (λ p : eta bool, p)
definition true ≔ imp false false

definition bnot (P: eta bool) ≔ imp P false
set prefix 5 "¬" ≔ bnot
definition band (P Q: eta bool) ≔ ¬ (imp P (¬ Q))
definition bor (P Q: eta bool) ≔ (imp (¬ P) Q)
set infix 6 "∧" ≔ band
set infix 5 "∨" ≔ bor
definition biff (P Q: eta bool) ≔ (imp P Q) ∧ (imp Q P)
