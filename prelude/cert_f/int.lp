require open encodings.cert_f prelude.cert_f.booleans prelude.cert_f.notequal

constant symbol integer : Term uType
definition int ≔ integer

// Should be inherited
symbol z : Term integer

definition nonzero_integer ≔ psub integer (λ x: Term integer, neq z x)
definition nzint ≔ nonzero_integer
