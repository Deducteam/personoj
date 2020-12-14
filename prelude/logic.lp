require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.logical
require open personoj.encodings.equality
require open personoj.encodings.prenex
require open personoj.encodings.builtins
//
// Propeans
// In [adlib.cert_f.bootstrap]

//
// Equalities
//

//
// Notequal
//
// definition neq {T: Set} (x y: El T) ≔ ¬ (x = y)
// symbol neq: χ (∀S (λt, scheme (t ~> t ~> prop)))
// rule neq _ $x $y ↪ ¬ ($x = $y)
// set infix left 2 "/=" ≔ neq
// set infix left 2 "≠" ≔ neq
/// Defined in builtins

//
// if_def
//

//
// propean_props
// Slightly modified from the prelude
constant symbol prop_exclusive: Prf (neq {prop} false true)
constant symbol prop_inclusive:
  Prf (∀ {prop} (λa, ((eq {prop} a false) ∨ (λ_, eq {prop} a true))))

theorem excluded_middle: Prf (∀ {prop} (λa, a ∨ (λ_, ¬ a)))
proof
  assume x f
  refine f
qed

//
// xor_def
//
definition xor (a b: El prop) ≔ neq {prop} a b

// PVS solves that kind of things thanks to the (bddsimp) tactic which uses an
// external C program
theorem xor_def:
  Prf (∀ {prop} (λa, ∀ {prop} (λb, eq {prop} (xor a b)
                                    (if {prop} a (λ_, ¬ b) (λ_, b)))))
proof
  set prover "Alt-Ergo"
  set prover_timeout 12
  assume a b p
  simpl
  assume hxor
admit
//
// Quantifier props[t: TYPE], built in
//
// Defined types
//
definition pred: El_k (∀K (λ_, scheme_k {|set|})) ≔ λt, t ~> prop
definition PRED ≔ pred
definition predicate ≔ pred
definition PREDICATE ≔ pred
definition setof ≔ pred
definition SETOF ≔ pred

//
// exists1
//

//
// equality_props
//
constant
symbol If_true
     : Prf (∀B (λt, ∀ (λx, ∀ {t} (λy, if true (λ_, x) (λ_, y) = x))))
constant
symbol If_false
     : Prf (∀B (λt, ∀ (λx, ∀ {t} (λy, if false (λ_, x) (λ_, y) = y))))

theorem if_same
      : Prf (∀B (λt, ∀ {prop} (λb, ∀ (λx: El t, if b (λ_, x) (λ_, x) = x))))
proof
admit

constant symbol reflexivity_of_equals: Prf (∀B (λt, ∀ (λx: El t, x = x)))

constant
symbol transitivity_of_equals
     : Prf (∀B (λt,
                ∀(λx: El t,
                    ∀(λy: El t,
                        ∀(λz: El t, (x = y) ∧ (λ_, y = z) ⊃ (λ_, x = z))))))

constant
symbol symmetry_of_equals
     : Prf (∀B (λt, ∀ (λx: El t, (∀ (λy: El t, (x = y) ⊃ (λ_, y = x))))))

// Not in prelude!
theorem eqind {t} x y: Prf (x = y) → Πp: El t → Prop, Prf (p y) → Prf (p x)
proof
  assume t x y heq
  apply symmetry_of_equals t x y heq
qed
set builtin "eqind" ≔ eqind

//
// if_props
//

theorem lift_if1:
  Prf (∀B (λs: Ty {|set|},
           ∀B (λt: Ty {|set|},
                 ∀ {prop}
                   (λa,
                      ∀ (λx: El s,
                           ∀ (λy: El s,
                                ∀ (λf: El (s ~> t),
                                     f (if a (λ_, x) (λ_, y))
                                     = if a (λ_, f x) (λ_, f y))))))))
proof
admit

theorem lift_if2:
  Prf (∀B (λs,
           ∀ {prop}
             (λa,
                ∀ {prop}
                  (λb,
                     ∀ {prop}
                       (λc,
                          ∀ (λx: El s,
                               ∀ (λy: El s,
                                    if (if {prop} a (λ_, b) (λ_, c))
                                       (λ_, x)
                                       (λ_, y)
                                     = if a
                                       (λ_, if b (λ_, x) (λ_, y))
                                       (λ_, if c (λ_, x) (λ_, y)))))))))
proof
admit
