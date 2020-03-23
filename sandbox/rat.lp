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

symbol trans {T} (x y z: Term T):
  Term (x = y) ⇒ Term (y = z) ⇒ Term (x = z)

// theorem one_neutral (a: Term N.nat) (b: Term N.nznat):
//   Term (rateq (times (a / b) (1 / onz)) (1 / onz))
// proof
// qed

// NOTE: we use this rewriting rule because in the proof below, calling simpl
// causes protected [opair] to appear, and we cannot use refl since it requires
// the user to input the protected opair, which is forbidden.
// Perhaps using [hints] could help, using [refl Nat _] and the unification
// engine would instantiate _ accordingly, but it is not likely since it is
// based on non linearity, and hints are linear.
// We rather reduce the proof to the trivial proof
rule &x = &x → true
theorem right_cancel (a: Term N.Nat) (b: Term N.Nznat):
    Term (rateq (times (a / b) ((fst b) / onz)) (a / onz))
proof
    assume a b
    // We execute simplifications one by one because [simpl] unfolds too much
    // reducing the beta redex [(λx, fst x) onz]
    refine
      trans
      (N.times (N.times a (fst b)) ((λx, fst x) onz))
      (N.times (N.times a (fst b)) (fst onz))
      (N.times ((λx, fst x) (N.nznat (N.times (fst b) (fst onz)) (N.prod_not_zero (fst b) (fst onz) (snd b) (snd onz)))) a)
      _ _
    simpl
    refine λx, x

    // reducing beta redex [(λx, fst x) ...]
    refine trans
      (N.times (N.times a (fst b)) (fst onz))
      (N.times (fst (N.nznat (N.times (fst b) (fst onz)) (N.prod_not_zero (fst b) (fst onz) (snd b) (snd onz)))) a)
      (N.times ((λx, fst x) (N.nznat (N.times (fst b) (fst onz)) (N.prod_not_zero (fst b) (fst onz) (snd b) (snd onz)))) a)
      _ _
    focus 1
    refine λx, x
    simpl

    refine N.prod_comm a (fst b)
qed

type Term (N.Nznat ⊑ N.Nat)
type λ(b: Term N.Nznat) (pr: Term (N.Nznat ⊑ N.Nat)),
  ↑ N.Nat pr b
//theorem cright_cancellation (a: Term N.Nat) (b: Term N.Nznat)
//  (pr: Term (N.Nznat ⊑ N.Nat)):
//  Term (rateq (times (a / b) ((↑ N.Nat pr b) / onz)) (a / onz))
//proof
//
//qed
// Should generate a TCC to provide [pr]

// theorem right_cancel (a b: Term N.Nat) ()
