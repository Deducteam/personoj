/// Dependent types

/// PVS allows dependent types using theory abstraction. For instance,
/// vector_real[n: nat]: THEORY BEGIN vec: TYPE END vector
/// allows to define
/// cons(n: nat, r: real, v: vector_real[n].vec): vector_real[n + 1].vec

require open personoj.encodings.lhol
             personoj.encodings.pvs_cert
             personoj.encodings.prenex;

constant symbol darr: Set → Kind → Kind;
symbol *> ≔ darr;
set notation *> infix right 6;
rule Ty (darr $t $k) ↪ El $t → Ty $k;
