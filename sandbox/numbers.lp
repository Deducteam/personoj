require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.prop_hol
require open personoj.encodings.prenex
require open personoj.prelude.logic
require open personoj.encodings.builtins
require open personoj.encodings.subtype_poly

//
// Theory numbers
//
constant symbol number: θ {|set|}
rule El (μ number) ↪ El number

//
// Theory number_fields
//
constant symbol field_pred: El (PRED number)
definition number_field ≔ psub field_pred
// number_field is an uninterpreted subtype
definition numfield ≔ number_field

symbol insertnum: El {|!Number!|} → El numfield

symbol {|+|}: El (numfield ~> numfield ~> numfield)
set infix left 6 "+" ≔ {|+|}
symbol {|-|}: El (numfield ~> numfield ~> numfield)
set infix 6 "-" ≔ {|-|}

// Other way to extend type of functions,
// symbol ty_plus: Term uType → Term uType → Term uType
// symbol polyplus {T: Term uType} {U: Term uType}
// : Term T → Term U → Term (ty_plus T U)
// // plus is defined on numfield
// rule ty_plus numfield numfield ↪ numfield

constant
symbol commutative_add : Prf (∀ (λx, ∀ (λy, x + y = y + x)))
constant
symbol associative_add
     : Prf (∀ (λx, ∀ (λy, ∀ (λz, x + (y + z) = (x + y) + z))))
// FIXME add a cast on zero?
// symbol identity_add (x: Term numfield): Term (x + zero = x)


//
// reals
//
constant symbol real_pred: El (pred numfield)
definition real ≔ psub real_pred

symbol Num_real: Prf (∀ (λx: El {|!Number!|}, real_pred (insertnum x)))

// Built in the PVS typechecker

// TODO: metavariables left
// definition nonzero_real ≔
//   psub {real}
//        (λx: El real,
//         neq {_} x (cast {_} {real} (λx, x) (insertnum 0) _))

// symbol closed_plus_real: Π(x y: Term real),
//   let pr ≔ S.restr numfield real_pred in
//   let xnf ≔ ↑ numfield pr x in
//   let ynf ≔ ↑ numfield pr y in
//   Term (real_pred (xnf + ynf))

// // hint Term $x ≡ Univ Type ↪ $x ≡ uType

// // With polymorphic plus
// rule ty_plus real real ↪ real

// symbol lt (x y: Term real): Term prop
// set infix 6 "<" ≔ lt
// definition leq (x y: Term real) ≔ (lt x y) ∨ (eq {real} x y)
// set infix 6 "<=" ≔ leq
// definition gt (x y: Term real) ≔ y < x
// set infix 7 ">" ≔ gt
// definition geq (x y: Term real) ≔ leq y x
// set infix 7 ">=" ≔ geq


// //
// // real_axioms
// //

// // ...

// //
// // rationals
// //
// symbol rational_pred: Term (pred real)
// definition rational ≔ Psub rational_pred
// definition rat ≔ rational
// theorem rational_not_empty: Term (∃ rational_pred)
// proof admit

// // Typically a TCC
// theorem rat_is_real: Term (rational ⊑ real)
// proof
//   refine S.restr real rational_pred
// qed

// // hint Psub $x ⊑ $y ≡ rational ⊑ real ↪ $x ≡ rational_pred, $y ≡ real
// theorem rat_is_real_auto: Term (rational ⊑ real)
// proof
//   apply S.restr _ _
// qed

// definition nonzero_rational_pred (x: Term rational): Term prop ≔
//   neq zero (↑ real rat_is_real x)
// definition nonzero_rational ≔ Psub nonzero_rational_pred
// definition nzrat ≔ nonzero_rational

// symbol closed_plus_rat (x y: Term rat):
//   let xreal ≔ ↑ real rat_is_real x in
//   let yreal ≔ ↑ real rat_is_real y in
//   let xnf ≔ ↑ numfield (S.restr numfield real_pred) xreal in
//   let ynf ≔ ↑ numfield (S.restr numfield real_pred) yreal in
//   let sum ≔ xnf + ynf in
//   Term (rational_pred (↓ real_pred sum (closed_plus_real xreal yreal)))
// rule ty_plus rat rat ↪ rat

// definition nonneg_rat_pred (x: Term rational) ≔ (↑ real rat_is_real x) >= zero
// definition nonneg_rat ≔ Psub nonneg_rat_pred

// theorem nonneg_rat_is_real: Term (nonneg_rat ⊑ real)
// proof
//   refine S.trans nonneg_rat rational real ?nnr_is_r ?r_is_r
//   focus 1
//   apply rat_is_real
//   apply S.restr rational nonneg_rat_pred
// qed

// definition posrat_pred (x: Term nonneg_rat) ≔ (↑ real nonneg_rat_is_real x) > zero
// definition posrat ≔ Psub posrat_pred

// symbol div: Term real → Term nonzero_real → Term real
// set infix left 8 "/" ≔ div

// /// NOTE: any expression of the type below would generate a TCC to prove
// // that posrat is a nzrat
// type λ (r: Term real) (q: Term posrat), r / (↑ nzreal _ q)
// theorem posrat_is_nzreal: Term (posrat ⊑ nzreal)
// proof
// admit
// // but PVS prefers to state that posrat is nzrat
// theorem posrat_is_nzrat: Term (posrat ⊑ nzrat)
// proof
//   refine S.sub nonzero_rational_pred posrat ?R ?P1 ?Fa
//   print
//   refine λx, x // Trivial proof that rational and posrat have the same root
//   refine S.trans posrat nonneg_rat rat ?R1 ?R2
//   apply S.restr nonneg_rat posrat_pred
//   apply S.restr rational nonneg_rat_pred
//   assume x
//   // TODO finish this proof
// admit
// type λ (r: Term real) (q: Term posrat), r /
//   (↑ nzreal _ (↑ nzrat posrat_is_nzrat q))
// ///

// //
// // integers
// //
// symbol integer_pred: Term (pred rational)
// definition integer ≔ Psub integer_pred
// // Proof of existence because NONEMPTY_TYPE
// theorem integer_not_empty: Term (∃ integer_pred)
// proof
// admit
// definition int ≔ integer
