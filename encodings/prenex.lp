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
symbol scheme_k: Kind → SchemeK
rule ϕ (scheme_k $X) ↪ θ $X

constant symbol ∀K: (Set → SchemeK) → SchemeK
rule ϕ (∀K $e) ↪ Πt: Set, ϕ ($e t)

symbol SchemeS: TYPE
symbol χ: SchemeS → TYPE
symbol scheme_s: Set → SchemeS
rule χ (scheme_s $X) ↪ η $X

constant symbol ∀S: (Set → SchemeS) → SchemeS
rule χ (∀S $e) ↪ Πt: Set, χ ($e t)

constant symbol ∀B: (Set → Bool) → Bool
rule ε (∀B $p) ↪ Πx: Set, ε ($p x)

// Since PVS supports dependent types through theory formal parameters, we allow
// prenex quantifications on elements “e: T” with “T: TYPE” in elements of type
// “Kind”.
set declared "∀k"
constant symbol ∀k {t: Set}: (η t → SchemeK) → SchemeK
rule ϕ (∀k {$t} $e) ↪ Πt: η $t, ϕ ($e t)
