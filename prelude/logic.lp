require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol
require open personoj.encodings.prenex
//
// Booleans
// In [adlib.cert_f.bootstrap]

//
// Equalities
//
// in [adlib.cert_f.bootstrap]

//
// Notequal
//
// definition neq {T: Set} (x y: η T) ≔ ¬ (x = y)
symbol neq: χ (∀S (λt, scheme (t ~> t ~> bool)))
rule neq _ $x $y ↪ ¬ ($x = $y)
set infix left 2 "/=" ≔ neq
set infix left 2 "≠" ≔ neq

//
// if_def
//

//
// boolean_props
// Slightly modified from the prelude
constant symbol bool_exclusive: ε (neq bool false true)
constant
symbol bool_inclusive
     : ε (forall {bool} (λa, ((eq {bool} a false) ∨ (λ_, eq {bool} a true))))

theorem excluded_middle: ε (forall {bool} (λa, a ∨ (λ_, ¬ a)))
proof
  assume x f
  refine f
qed

//
// xor_def
//
definition xor (a b: η bool) ≔ neq bool a b

set flag "print_implicits" on
theorem xor_def: ε (forall
                      {bool}
                      (λa,
                         forall
                           {bool}
                           (λb, eq {bool} (xor a b)
                                   (if {bool} a (λ_, ¬ b) (λ_, b)))))
proof
admit


//
// Quantifier props[t: TYPE]
//
set declared "∃"
definition ∃ {T: Set} (P: η T → η bool) ≔ ¬ (forall (λx, ¬ (P x)))

//
// Defined types
//
// FIXME: needs another prenex polymorphism to be encoded,
// ∀K (λt, ? (t ~> {|set|})) ≔ λt: ϕ {|set|}, t ~> bool
// definition pred : χ (∀S (λt, scheme (t ~> {|set|}))) ≔ λt: ϕ {|set|}, t ~> bool
definition pred t ≔ t ~> bool
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

set debug +ui
constant
symbol If_true
     : ε (∀B
           (λt, forall
                  (λx, forall {t} (λy, if true (λ_, x) (λ_, y) = x))))
constant
symbol If_false
     : ε (∀B
           (λt, forall
                  (λx, forall {t} (λy, if false (λ_, x) (λ_, y) = y))))

theorem if_same
      : ε (∀B (λt,
                 forall {bool} (λb, forall (λx: η t,
                                              if b (λ_, x) (λ_, x) = x))))
proof
admit

constant symbol reflexivity_of_equals: ε (∀B (λt, forall (λx: η t, x = x)))
set builtin "refl" ≔ reflexivity_of_equals

constant
symbol transitivity_of_equals
     : ε (∀B (λt,
                forall
                  (λx: η t,
                     forall
                       (λy: η t,
                          forall
                            (λz: η t, (x = y) ∧ (λ_, y = z) ⊃ (λ_, x = z))))))

constant
symbol symmetry_of_equals
     : ε (∀B (λt, forall (λx: η t, (forall (λy: η t, (x = y) ⊃ (λ_, y = x))))))

//
// if_props
//
