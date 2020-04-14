// Compute the term [⊥ ⊃ 1 / 0 = 1 / 0]. The principle is that [1/0] is well
// typed because in the right part of the implication, false is assumed.
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.equality
require open personoj.encodings.booleans

symbol imp: ∀P: Bool, (ε P ⇒ η bool) ⇒ η bool
set infix left 6 "⊃" ≔ imp

require personoj.paper.rat as Q

set builtin "0" ≔ Q.Z
set builtin "+1" ≔ Q.S

theorem zisnotz (h: ε false): ε (Q.{|nznat?|} 0)
proof
  assume Hf _
  refine Hf
qed

// Computes ⊥ ⊃ (1 / 0) = 1 / 0
compute false ⊃ (λh,
                 let ooz ≔ Q.div 1 (pair 0 (zisnotz h)) in
                 eq ooz ooz)
