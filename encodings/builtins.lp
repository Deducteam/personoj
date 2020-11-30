require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.logical
require open personoj.encodings.equality
require open personoj.encodings.prenex

constant symbol {|!Number!|}: Set

constant symbol zero: El {|!Number!|}
constant symbol succ: El ({|!Number!|} ~> {|!Number!|})

set builtin "0" ≔ zero
set builtin "+1" ≔ succ

// In PVS, the axipms 1 ≠ 2, 1≠3, ... are built in
// Here we use the decidable equality
rule Prf (succ $n = succ $m) ↪ Prf ($n = $m)
with Prf (zero = succ _) ↪ Prf false
with Prf (zero = zero) ↪ Prf true

// Define strings as list of numbers
constant symbol {|!String!|}: Set
constant symbol str_empty: El {|!String!|}
constant symbol str_cons: El {|!Number!|} → El {|!String!|} → El {|!String!|}

set declared "∃"
definition ∃ {T: Set} (P: El T → El bool) ≔ ¬ (∀ (λx, ¬ (P x)))

symbol propositional_extensionality:
  Prf (∀ {bool} (λp, (∀ {bool} (λq, (iff p q) ⊃ (λ_, eq {bool} p q)))))

definition neq {t} x y ≔ ¬ (eq {t} x y)
set infix left 2 "/=" ≔ neq
set infix left 2 "≠" ≔ neq
