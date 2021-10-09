/// Dependent types

/// PVS allows dependent types using theory abstraction. For instance,
/// vector_real[n: nat]: THEORY BEGIN vec: TYPE END vector
/// allows to define
/// cons(n: nat, r: real, v: vector_real[n].vec): vector_real[n + 1].vec

require open personoj.lhol
             personoj.pvs_cert;

constant symbol Kind: TYPE;
constant symbol set: Kind;

injective symbol Ty : Kind → TYPE;
rule Ty set ↪ Set;

symbol *> : Set → Kind → Kind;
notation *> infix right 6;
rule Ty ($t *> $k) ↪ El $t → Ty $k;
