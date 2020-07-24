/// Prenex polymorphism

/// Prenex polymorphism allows to quantify over variables of type ‘Set’.
/// PVS allows it in theory formals, such as
/// groups[t: TYPE]: THEORY BEGIN ... END groups
/// For more informations on the encoding of prenex polymorphism, see
/// https://arxiv.org/abs/1807.01873 and theory U
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

constant symbol SchemeK: TYPE
symbol ϕ: SchemeK → TYPE
constant symbol scheme_k: Kind → SchemeK
rule ϕ (scheme_k $X) ↪ θ $X

constant symbol ∀K: (Set → SchemeK) → SchemeK
rule ϕ (∀K $e) ↪ Πt: Set, ϕ ($e t)

constant symbol SchemeS: TYPE
symbol χ: SchemeS → TYPE
constant symbol scheme_s: Set → SchemeS
rule χ (scheme_s $X) ↪ El $X

constant symbol ∀S: (Set → SchemeS) → SchemeS
rule χ (∀S $e) ↪ Πt: Set, χ ($e t)

constant symbol ∀B: (Set → Bool) → Bool
rule Prf (∀B $p) ↪ Πx: Set, Prf ($p x)
