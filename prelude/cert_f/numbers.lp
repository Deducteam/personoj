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
constant symbol number_field : Term (ePsub number field_pred)
// number_field is an uninterpreted subtype
definition numfield ≔ number_field

constant symbol zero : Term number

//
// reals
//
symbol real_pred: Term (pred numfield)
definition real ≔ ePsub numfield real_pred
theorem real_not_empty: Term (∃ real_pred)
proof admit

symbol lt (x y: Term real): Term bool
set infix 6 "<" ≔ lt
definition leq (x y: Term real) ≔ (lt x y) ∨ (@eq real x y)
definition gt (x y: Term real) ≔ y < x
set infix 7 ">" ≔ gt
definition geq (x y: Term real) ≔ leq y x


//
// real_axioms
//

// ...

//
// rationals
//
constant symbol rational: Term uType

//
// integers
//
symbol integer_pred: Term (pred rational)
// constant symbol integer: Term (∃ integer_pred) ⇒ Term uType
constant symbol integer: Term uType
// Proof of existence because NONEMPTY_TYPE
theorem integer_not_empty: Term (∃ integer_pred)
proof
admit
definition int ≔ integer

symbol natz : Term int

definition nonzero_integer ≔ ePsub int (λx, neq natz x)
definition nzint ≔ nonzero_integer
