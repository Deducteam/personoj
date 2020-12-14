/// Prenex polymorphism

/// Prenex polymorphism allows to quantify over variables of type ‘Set’.
/// PVS allows it in theory formals, such as
/// groups[t: TYPE]: THEORY BEGIN ... END groups
/// For more informations on the encoding of prenex polymorphism, see
/// https://arxiv.org/abs/1807.01873 and theory U
require open personoj.encodings.lhol

// Quantification for objects of type ‘Kind’
set declared "∀K"
// Quantification for objects of type ‘Set’
set declared "∀S"
// Quantification for objects of type ‘Prop’
set declared "∀B"

// To interpret PVS sorts
injective symbol Ty : Kind → TYPE
rule Ty {|set|} ↪ Set

constant symbol SchemeK: TYPE
injective symbol El_k: SchemeK → TYPE
constant symbol scheme_k: Kind → SchemeK
rule El_k (scheme_k $X) ↪ Ty $X

constant symbol ∀K: (Set → SchemeK) → SchemeK
rule El_k (∀K $e) ↪ Πt: Set, El_k ($e t)

constant symbol SchemeS: TYPE
injective symbol El_s: SchemeS → TYPE
constant symbol scheme_s: Set → SchemeS
rule El_s (scheme_s $X) ↪ El $X

constant symbol ∀S: (Set → SchemeS) → SchemeS
rule El_s (∀S $e) ↪ Πt: Set, El_s ($e t)

constant symbol ∀B: (Set → Prop) → Prop
rule Prf (∀B $p) ↪ Πx: Set, Prf ($p x)
