// Deep encoding of terms carrying their types.
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans

/// Type carried with  the encoded terms
constant symbol t: Set → TYPE
constant symbol quote {T: Set}: η T → t T
injective symbol unquote {T: Set}: t T → η T
rule unquote (quote $t) ↪ $t

// Coding of applied types
constant symbol APP: Set → ΠU: Set, t U → Set
rule η (APP (arrd {$T} $bU) $T $y) ↪ η ($bU (unquote $y))

// Quoted terms application
constant symbol app {T: Set} {S: Set}: t T → Πy: t S, t (APP T S y)
rule unquote (app {arrd {$a} $b} {$a} $t $u) ↪ (unquote $t) (unquote $u)

constant symbol p: Bool → TYPE
constant symbol quote_p {P: Bool}: ε P → p P
symbol unquote_p {P: Bool}: p P → ε P
