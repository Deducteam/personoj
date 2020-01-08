require open pvs_core

set builtin "T" ≔ eta
set builtin "P" ≔ eps

// booleans
definition bool ≔ Prop
constant symbol false : eta bool
constant symbol true : eta bool

symbol bnot : eta bool ⇒ eta bool
symbol band : eta bool ⇒ eta bool ⇒ eta bool
symbol bor : eta bool ⇒ eta bool ⇒ eta bool
symbol bimpl : eta bool ⇒ eta bool ⇒ eta bool
symbol biff : eta bool ⇒ eta bool ⇒ eta bool
