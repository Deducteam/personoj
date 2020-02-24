require open encodings.cert_f prelude.cert_f.booleans
prelude.cert_f.equalities prelude.cert_f.notequal
prelude.cert_f.naturalnumbers prelude.cert_f.int
require adlib.cert_f.nat as N

constant symbol rat : Univ Type
constant symbol zero : Term rat

symbol frac : Term nat ⇒ Term N.nznat ⇒ Term rat
set infix 8 "/" ≔ frac

symbol times : @Term Type rat ⇒ @Term Type rat ⇒ @Term Type rat

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

theorem rrefl (a: Term nat) (b: Term N.nznat):
  Term (rateq (a / b) (a / b))
proof
assume a b
apply N.prod_comm a (fst N.not_zero b)
qed

theorem one_neutral (a: Term nat) (b: Term N.nznat):
  Term (rateq (times (a / b) (1 / onz)) (1 / onz))
proof
assume a b
refine N.prod_comm ?p1[a,b] ?p2[a,b]
qed
// theorem right_cancellation (a: Term nat) (b: Term N.nznat):
//   Term (rateq (times (a / b) ((fst N.not_zero b) / onz)) (a / onz))
// proof
// assume a b
// refine Term
// qed
