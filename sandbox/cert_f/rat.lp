require open encodings.cert_f adlib.cert_f.booleans
prelude.cert_f.logic
require adlib.cert_f.nat as N

set builtin "0" ≔ N.zero
set builtin "+1" ≔ N.succ

constant symbol rat : Univ Type
constant symbol zero : Term rat

symbol frac : Term N.nat ⇒ Term N.nznat ⇒ Term rat
set infix 8 "/" ≔ frac

symbol times : Term rat ⇒ Term rat ⇒ Term rat

rule times (frac &a &b) (frac &c &d) →
  let bv ≔ fst N.not_zero &b in
  let dv ≔ fst N.not_zero &d in
  frac
   (N.times &a &c)
   (N.nznatc
    (N.times bv dv)
    (N.prod_not_zero bv dv
     (snd N.not_zero &b)
     (snd N.not_zero &d)))

symbol rateq : Term rat ⇒ Term rat ⇒ Univ Prop
rule rateq (&a / &b) (&c / &d) →
  let nzval x ≔ fst N.not_zero x in
  eq (N.times &a (nzval &d)) (N.times (nzval &b) &c)

definition onz : Term N.nznat ≔ N.nznatc 1 N.one_not_zero

theorem rrefl (a: Term N.nat) (b: Term N.nznat):
  Term (rateq (a / b) (a / b))
proof
assume a b
apply N.prod_comm a (fst N.not_zero b)
qed

// theorem one_neutral (a: Term N.nat) (b: Term N.nznat):
//   Term (rateq (times (a / b) (1 / onz)) (1 / onz))
// proof
// qed
type Term (N.nznat ⊑ N.nat)
type λ(b: Term N.nznat) (pr: Term (N.nznat ⊑ N.nat)),
  @↑ N.nznat N.nat pr b
// FIXME explicitness required
theorem right_cancellation (a: Term N.nat) (b: Term N.nznat)
  (pr: Term (N.nznat ⊑ N.nat)):
  Term (rateq (times (a / b) ((@↑ N.nznat N.nat pr b) / onz)) (a / onz))
proof
qed
// Should generate a TCC to provide [pr]
