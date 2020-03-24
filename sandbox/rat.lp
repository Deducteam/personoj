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

// NOTE: we use this rewriting rule because in the proof below, calling simpl
// causes protected [opair] to appear, and we cannot use refl since it requires
// the user to input the protected opair, which is forbidden.
// Perhaps using [hints] could help, using [refl Nat _] and the unification
// engine would instantiate _ accordingly, but it is not likely since it is
// based on non linearity, and hints are linear.
// We rather reduce the proof to the trivial proof
rule &x = &x → true
theorem right_cancellation (a: Term N.Nat) (b: Term N.Nznat):
    Term (rateq (times (a / b) ((fst b) / onz)) (a / onz))
proof
    assume a b
    simpl
    refine N.prod_comm a (fst b)
qed
