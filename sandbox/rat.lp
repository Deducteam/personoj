require open
    personoj.encodings.cert_f
    personoj.adlib.bootstrap
    personoj.prelude.logic

require personoj.sandbox.nat as N

set builtin "0" ≔ N.zero
set builtin "+1" ≔ N.succ

constant symbol Rat : Term uType
constant symbol zero : Term Rat

symbol rat : Term N.Nat ⇒ Term N.Nznat ⇒ Term Rat
set infix 8 "/" ≔ rat

symbol times : Term Rat ⇒ Term Rat ⇒ Term Rat

type λx: Term N.Nznat, fst x

rule times (rat &a &b) (rat &c &d) →
  let bv ≔ fst &b in
  let dv ≔ fst &d in
  rat
   (N.times &a &c)
   (N.nznat
    (N.times bv dv)
    (N.prod_not_zero bv dv
     (snd &b)
     (snd &d)))

symbol rateq : Term Rat ⇒ Term Rat ⇒ Term bool
rule rateq (&a / &b) (&c / &d) →
  let nzval x ≔ fst x in
  (N.times &a (nzval &d)) = (N.times (nzval &b) &c)

definition onz : Term N.Nznat ≔ N.nznat 1 N.one_not_zero

theorem rrefl (a: Term N.Nat) (b: Term N.Nznat):
  Term (rateq (a / b) (a / b))
proof
    assume a b
    apply N.prod_comm a (fst b)
qed

// theorem one_neutral (a: Term N.nat) (b: Term N.nznat):
//   Term (rateq (times (a / b) (1 / onz)) (1 / onz))
// proof
// qed
type Term (N.Nznat ⊑ N.Nat)
type λ(b: Term N.Nznat) (pr: Term (N.Nznat ⊑ N.Nat)),
  ↑ N.Nat pr b
theorem right_cancellation (a: Term N.Nat) (b: Term N.Nznat)
  (pr: Term (N.Nznat ⊑ N.Nat)):
  Term (rateq (times (a / b) ((↑ N.Nat pr b) / onz)) (a / onz))
proof
qed
// Should generate a TCC to provide [pr]

// theorem right_cancel (a b: Term N.Nat) ()
