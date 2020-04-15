// Compute the term [⊥ ⊃ 1 / 0 = 1 / 0]. The principle is that [1/0] is well
// typed because in the right part of the implication, false is assumed.
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.equality
require open personoj.encodings.bool_hol

symbol imp: ∀P: Bool, (ε P ⇒ η bool) ⇒ η bool
set infix left 6 "⊃" ≔ imp

require personoj.paper.rat as Q

set builtin "0" ≔ Q.Z
set builtin "+1" ≔ Q.S

theorem exfalso (_: ε false) (P: Bool): ε P
proof
  assume false P
  refine false P
qed

// Computes ⊥ ⊃ (1/0 = 1/0)
compute false ⊃ (λh,
                 let ooz ≔ Q.div 1 (pair 0 (exfalso h (Q.{|nznat?|} 0))) in
                 eq ooz ooz)

definition Qone ≔ Q.div 1 (pair 1 (Q.s_not_z 0))

// If ⊥ then 1/0 else 1/1
compute if false (λh: ε false,
                  let z ≔ pair 0 (exfalso h (Q.{|nznat?|} 0)) in
                  Q.div 1 z)
                 (λ_, Qone)

// TODO: some work with excluded middle to have the same proposition
// as above but with [if true] and reversed arguments.
