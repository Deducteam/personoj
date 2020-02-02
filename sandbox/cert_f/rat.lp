require open encodings.cert_f prelude.cert_f.booleans
prelude.cert_f.equalities prelude.cert_f.notequal
prelude.cert_f.naturalnumbers prelude.cert_f.int
require adlib.cert_f.nat as N

constant symbol rat : Univ Type
constant symbol zero : Term rat

symbol frac : Term nat ⇒ Term N.nznat ⇒ Term rat

symbol times : @Term Type rat ⇒ @Term Type rat ⇒ @Term Type rat

rule times (frac &a &b) (frac &c &d) →
  frac
   (N.times &a &c)
   (N.nznatc
    (N.times (fst N.not_zero &b) (fst N.not_zero &d))
    (N.prod_not_zero
     (fst N.not_zero &b)
     (fst N.not_zero &d)
     (snd N.not_zero &b)
     (snd N.not_zero &d)))

symbol rateq : Term rat ⇒ Term rat ⇒ Univ Prop
rule rateq (frac &a &b) (frac &c &d) →
  eq (N.times &a (fst N.not_zero &d)) (N.times (fst N.not_zero &b) &c)

definition onz : Term N.nznat ≔ N.nznatc 1 N.one_not_zero

theorem right_cancellation (a: Term nat) (b: Term N.nznat):
  Term (rateq (times (frac a b) (frac (fst N.not_zero b) onz)) (frac a onz))
proof
assume a b
simpl
refine N.prod_comm a b
qed
