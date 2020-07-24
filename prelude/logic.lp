require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol
require open personoj.encodings.prenex
require open personoj.encodings.builtins
//
// Booleans
// In [adlib.cert_f.bootstrap]

//
// Equalities
//

//
// Notequal
//
// definition neq {T: Set} (x y: El T) ≔ ¬ (x = y)
// symbol neq: χ (∀S (λt, scheme (t ~> t ~> bool)))
// rule neq _ $x $y ↪ ¬ ($x = $y)
// set infix left 2 "/=" ≔ neq
// set infix left 2 "≠" ≔ neq
/// Defined in builtins

//
// if_def
//

//
// boolean_props
// Slightly modified from the prelude
constant symbol bool_exclusive: Prf (neq {bool} false true)
constant symbol bool_inclusive:
  Prf (∀ {bool} (λa, ((eq {bool} a false) ∨ (λ_, eq {bool} a true))))

theorem excluded_middle: Prf (∀ {bool} (λa, a ∨ (λ_, ¬ a)))
proof
  assume x f
  refine f
qed

//
// xor_def
//
definition xor (a b: El bool) ≔ neq {bool} a b

// PVS solves that kind of things thanks to the (bddsimp) tactic which uses an
// external C program
theorem xor_def:
  Prf (∀ {bool} (λa, ∀ {bool} (λb, eq {bool} (xor a b)
                                    (if {bool} a (λ_, ¬ b) (λ_, b)))))
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
definition pred: ϕ (∀K (λ_, scheme_k {|set|})) ≔ λt, t ~> bool
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
      : Prf (∀B (λt, ∀ {bool} (λb, ∀ (λx: El t, if b (λ_, x) (λ_, x) = x))))
proof
admit

constant symbol reflexivity_of_equals: Prf (∀B (λt, ∀ (λx: El t, x = x)))
set builtin "refl" ≔ reflexivity_of_equals

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
theorem eqind {t} x y: Prf (x = y) → Πp: El t → Bool, Prf (p y) → Prf (p x)
proof
  assume t x y heq
  apply symmetry_of_equals t x y heq
qed
set builtin "eqind" ≔ eqind

//
// if_props
//

theorem lift_if1:
  Prf (∀B (λs: θ {|set|},
           ∀B (λt: θ {|set|},
                 ∀ {bool}
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
           ∀ {bool}
             (λa,
                ∀ {bool}
                  (λb,
                     ∀ {bool}
                       (λc,
                          ∀ (λx: El s,
                               ∀ (λy: El s,
                                    if (if {bool} a (λ_, b) (λ_, c))
                                       (λ_, x)
                                       (λ_, y)
                                     = if a
                                       (λ_, if b (λ_, x) (λ_, y))
                                       (λ_, if c (λ_, x) (λ_, y)))))))))
proof
admit
