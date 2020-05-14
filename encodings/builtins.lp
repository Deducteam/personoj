require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

constant symbol {|!Number!|}: θ {|set|}

constant symbol zero: η {|!Number!|}
constant symbol succ: η ({|!Number!|} ~> {|!Number!|})

set builtin "0" ≔ zero
set builtin "+1" ≔ succ

// In PVS, the axipms 1 ≠ 2, 1≠3, ... are built in
// Here we use the decidable equality
rule ε (succ $n = succ $m) ↪ ε ($n = $m)
with ε (zero = succ _) ↪ ε false
with ε (zero = zero) ↪ ε true

// Define strings as list of numbers
constant symbol {|!String!|}: θ {|set|}
constant symbol str_empty: η {|!String!|}
constant symbol str_cons: η {|!Number!|} → η {|!String!|} → η {|!String!|}

set declared "∃"
definition ∃ {T: Set} (P: η T → η bool) ≔ ¬ (∀ (λx, ¬ (P x)))

symbol propositional_extensionality:
  ε (∀ {bool} (λp, (∀ {bool} (λq, (iff p q) ⊃ (λ_, eq {bool} p q)))))

definition neq {t} x y ≔ ¬ (eq {t} x y)
set infix left 2 "/=" ≔ neq
set infix left 2 "≠" ≔ neq
