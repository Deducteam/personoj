/// Dependent types

/// PVS allows dependent types using theory abstraction. For instance,
/// vector[t: TYPE, n: nat]: THEORY BEGIN vec: TYPE END vector
/// allows to define
/// cons(n: nat, e: t, v: vector[t, n].vec): vector[t, n + 1].vec

require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

constant symbol darr: Set → Kind → Kind
set infix right 6 "*>" ≔ darr
rule θ ($t *> $k) ↪ η $t → θ $k
