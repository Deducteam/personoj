/// Dependent types

/// PVS allows dependent types using theory abstraction. For instance,
/// vector_real[n: nat]: THEORY BEGIN vec: TYPE END vector
/// allows to define
/// cons(n: nat, r: real, v: vector_real[n].vec): vector_real[n + 1].vec

require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

constant symbol darr: Set → Kind → Kind
set infix right 6 "*>" ≔ darr
rule θ ($t *> $k) ↪ η $t → θ $k
