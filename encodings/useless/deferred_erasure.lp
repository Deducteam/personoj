// Deep encoding using type erasure. Deferred terms do not carry their type, but
// the unquoting function takes a type as argument and unquotes the term
// according to this type.
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

constant symbol t: TYPE
constant symbol quote {T: Set}: η T → t
injective symbol app: t → t → t
constant symbol abs: (t → t) → t

rule app (abs $b) $a ↪ $b $a

symbol unquote (T: Set): t → η T
symbol quote_B {P: Bool}: ε P → t
symbol unquote_B (P: Bool): t → ε P

rule unquote (arrd $b) $f $x ↪ unquote ($b $x) (app $f (quote $x))
with unquote_B (impd $b) $h $x ↪ unquote_B ($b $x) (app $h (quote_B $x))
with unquote_B (forall $b) $f $x ↪ unquote_B ($b $x) (app $f (quote $x))

symbol if (T: Set): Bool → t → t → η T
rule if $T true $then _ ↪ unquote $T $then
with if $T false _ $else ↪ unquote $T $else
