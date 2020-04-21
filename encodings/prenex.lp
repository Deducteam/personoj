// Prenex polymorphism
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

set declared "χ"
set declared "∀S"
set declared "∀B"

symbol Scheme: TYPE
symbol χ: Scheme → TYPE
symbol scheme: Set → Scheme
rule χ (scheme $X) ↪ η $X

constant symbol ∀S: (Set → Scheme) → Scheme
rule χ (∀S $p) ↪ ΠT: Set, χ ($p T)

constant symbol ∀B: (Set → Bool) → Bool
rule ε (∀B $P) ↪ Πx: Set, ε ($P x)
