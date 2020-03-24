require open
    personoj.encodings.cert_f
    personoj.adlib.bootstrap
    personoj.prelude.logic

constant symbol Nat: Term uType
injective symbol succ: Term Nat ⇒ Term Nat
constant symbol zero: Term Nat
set builtin "0" ≔ zero
set builtin "+1" ≔ succ

symbol times : Term Nat ⇒ Term Nat ⇒ Term Nat
symbol plus : Term Nat ⇒ Term Nat ⇒ Term Nat
set infix left 6 "+" ≔ plus
set infix left 7 "*" ≔ times

rule (succ &n) +       &m  → succ (&n + &m)
 and        0  +       &m  → &m
 and       &n  + (succ &m) → succ (&n + &m)
 and       &n  +        0  → &n

rule (succ &n) *       &m  → &n * &m + &m
 and        0  *        _  → 0
 and       &n  * (succ &m) → &n * &m + &n
 and        _  * 0         → 0

symbol prod_comm (x y: Term Nat): Term ((x * y) = (y * x))


//
// Non zero naturals
//

definition not_zero ≔ neq 0
symbol prod_not_zero (x y: Term Nat):
  Term (not_zero x) ⇒ Term (not_zero y) ⇒ Term (not_zero (times x y))

definition Nznat ≔ Psub not_zero

// Constructor of nznat
definition nznat (x: Term Nat) (h: Term (not_zero x)) : Term Nznat ≔
  pair not_zero x h

symbol one_not_zero: Term (not_zero 1)

symbol induction (P: Term Nat ⇒ Term bool):
  ∀n, Term (P 0) ⇒ Term (P (n + 1)) ⇒ ∀m, Term (P m)

// Divisions
definition div (x y: Term Nat) ≔ ∃ (λk, x * k = y)
definition even (x: Term Nat) ≔ div 2 x
definition Even ≔ Psub even

theorem even_stable_double: ∀x: Term Even, Term (even (2 * (fst x)))
proof
    assume x h
    refine (h (fst x) _)
    simpl
    refine reflexivity_of_equal _ ((fst x) + (fst x))
qed

symbol surjective_pairing T (p: Term T ⇒ Term bool):
       ∀x: Term (Psub p), Term (pair p (fst x) (snd x) = x)

//symbol app_thm (D R: Term uType) (f: Term (D ~> R))
//     : ∀x y: Term D, Term (x = y) ⇒ Term (f x = f y)

theorem fst_injective: ∀x y: Term Even, Term (fst x = fst y) ⇒ Term (x = y)
proof
    assume x y h
    refine transitivity_of_equal Even x (pair even (fst x) (snd x)) y _
    assume h0
    refine h0 ?P0[x,y,h,h0] _

    // Proof of P0 that x = pair even (fst x) (snd x)
    refine symmetry_of_equal Even (pair even (fst x) (snd x)) x _
    refine surjective_pairing _ even x

    refine transitivity_of_equal Even (pair even (fst x) (snd x)) (pair even (fst y) (snd y)) y _
    assume h1
    refine h1 _ _
    focus 1
    refine surjective_pairing _ even y

abort

rule &x = &x → true
theorem prf_irrelevance T (p: Term T ⇒ Term bool)
: ∀(x: Term T) (pr: Term (p x)) (pr': Term (p x)), Term (pair p x pr = pair p x pr')
proof
    assume T p x pr0 pr1
    simpl
    assume h
    refine h
qed
