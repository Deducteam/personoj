require adlib.cert_f.subtype as S
require open encodings.cert_f
adlib.cert_f.booleans
prelude.cert_f.logic

//
// Theory numbers
//
constant symbol number: Term uType

//
// Theory number_fields
//
symbol field_pred: Term number ⇒ Univ Prop
definition number_field ≔ ePsub number field_pred
// number_field is an uninterpreted subtype
definition numfield ≔ number_field


//
// reals
//
symbol real_pred: Term (pred numfield)
definition real ≔ ePsub numfield real_pred
theorem real_not_empty: Term (∃ real_pred)
proof admit

constant symbol zero : Term real
// Built in the PVS typechecker

definition nonzero_real_pred ≔ neq zero
definition nonzero_real ≔ ePsub real nonzero_real_pred
theorem nonzero_real_not_empty: Term (∃ nonzero_real_pred)
proof admit
definition nzreal ≔ nonzero_real


symbol lt (x y: Term real): Term bool
set infix 6 "<" ≔ lt
definition leq (x y: Term real) ≔ (lt x y) ∨ (eq {real} x y)
set infix 6 "<=" ≔ leq
definition gt (x y: Term real) ≔ y < x
set infix 7 ">" ≔ gt
definition geq (x y: Term real) ≔ leq y x
set infix 7 ">=" ≔ geq


//
// real_axioms
//

// ...

//
// rationals
//
symbol rational_pred: Term (pred real)
definition rational ≔ ePsub real rational_pred
theorem rational_not_empty: Term (∃ rational_pred)
proof admit

// Typically a TCC
theorem rat_is_real: Term (rational ⊑ real)
proof
  refine S.restr real rational_pred
qed

definition nonzero_rational_pred (x: Term rational): Term bool ≔
  neq zero (↑ real rat_is_real x)
definition nonzero_rational ≔ ePsub rational nonzero_rational_pred
definition nzrat ≔ nonzero_rational


definition nonneg_rat_pred (x: Term rational) ≔ (↑ real rat_is_real x) >= zero
definition nonneg_rat ≔ ePsub rational nonneg_rat_pred

theorem nonneg_rat_is_real: Term (nonneg_rat ⊑ real)
proof
  refine S.trans nonneg_rat rational real ?nnr_is_r ?r_is_r
  focus 1
  apply rat_is_real
  apply S.restr rational nonneg_rat_pred
qed

definition posrat_pred (x: Term nonneg_rat) ≔ (↑ real nonneg_rat_is_real x) > zero
definition posrat ≔ ePsub nonneg_rat posrat_pred

symbol div: Term real ⇒ Term nonzero_real ⇒ Term real
set infix left 8 "/" ≔ div

/// NOTE: any expression of the type below would generate a TCC to prove
// that posrat is a nzrat
type λ (r: Term real) (q: Term posrat), r / (↑ nzreal _ q)
theorem posrat_is_nzreal: Term (posrat ⊑ nzreal)
proof
admit
// but PVS prefers to state that posrat is nzrat
theorem posrat_is_nzrat: Term (posrat ⊑ nzrat)
proof
  // FIXME explicitness behaves strangely
  refine @S.sub nonneg_rat rational
    posrat_pred nonzero_rational_pred ?Carrier ?Imp
  apply S.restr rational nonneg_rat_pred
  assume x Pxgz Pxez
  // Some work needed on casts
  admit
type λ (r: Term real) (q: Term posrat), r /
  (↑ nzreal _ (↑ nzrat posrat_is_nzrat q))
///

//
// integers
//
symbol integer_pred: Term (pred rational)
definition integer ≔ ePsub rational integer_pred
// Proof of existence because NONEMPTY_TYPE
theorem integer_not_empty: Term (∃ integer_pred)
proof
admit
definition int ≔ integer

symbol natz : Term int

definition nonzero_integer ≔ ePsub int (neq natz)
definition nzint ≔ nonzero_integer
