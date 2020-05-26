// Prenex polymorphism: prenex quantification on elements of type ‘Set’.
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

// Quantification for objects of type ‘Kind’
set declared "ϕ"
set declared "∀K"
// Quantification for objects of type ‘Set’
set declared "χ"
set declared "∀S"
// Quantification for objects of type ‘Bool’
set declared "∀B"

symbol SchemeK: TYPE
symbol ϕ: SchemeK → TYPE
symbol schemeK: Kind → SchemeK
rule ϕ (schemeK $X) ↪ θ $X

constant symbol ∀K: (Set → SchemeK) → SchemeK
rule ϕ (∀K $e) ↪ Πt: Set, ϕ ($e t)

symbol Scheme: TYPE
symbol χ: Scheme → TYPE
symbol scheme: Set → Scheme
rule χ (scheme $X) ↪ η $X

constant symbol ∀S: (Set → Scheme) → Scheme
rule χ (∀S $e) ↪ Πt: Set, χ ($e t)

constant symbol ∀B: (Set → Bool) → Bool
rule ε (∀B $p) ↪ Πx: Set, ε ($p x)
