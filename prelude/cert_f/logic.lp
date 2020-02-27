require open encodings.cert_f
adlib.cert_f.booleans
require adlib.cert_f.induction as I

//
// Booleans
// In [adlib.cert_f.booleans]

//
// Equalities
//
symbol eq {T: Term uType}: Term T ⇒ Term T ⇒ Term uProp

//
// Notequal
//
definition neq {T: Term uType} (x y: Term T) ≔ bnot (eq x y)
set infix left 6 "/=" ≔ neq
set declared "≠"
set infix left 6 "≠" ≔ neq

//
// if_def
//
symbol if {T: Term uType} : Term uProp ⇒ Term T ⇒ Term T ⇒ Term T
// The reduction rules for if are in [equality_props]

//
// boolean_props
// Slightly modified from the prelude
constant symbol bool_exclusive: Term (@neq bool false true)
constant symbol bool_inclusive A: Term ((@eq bool A false) ∨ (@eq bool A true))

theorem excluded_middle (A: Term bool): Term (A ∨ ¬ A)
proof
admit

//
// xor_def
//
// FIXME explicitness required
definition xor (a b: Term bool) ≔ @neq bool a b

set flag "print_implicits" on
// FIXME explicitness required
theorem xor_def (a b: Term bool):
  Term (@eq bool (xor a b) (@if bool a (bnot b) b))
proof
refine I.disjunction
  (λa: Term bool, @forall bool (λb, @eq bool (xor a b) (@if bool a (bnot b) b)))
  ?Cf ?Ct
refine I.disjunction
  (λ b, @eq bool (xor false b) (@if bool false (bnot b) b))
  ?Ccf ?Cct
admit

//
// Quantifier props
//
set declared "∃"
// Declared as a lemma in the prelude
definition ∃ {eT: Term uType} (P: Term eT ⇒ Term bool) ≔
  Term (¬ (forall (λx, ¬ (P x))))

//
// Defined types
//
definition pred (eT: Term uType) ≔ Term eT ⇒ Term bool
definition PRED (eT: Term uType) ≔ Term eT ⇒ Term bool
definition predicate (eT: Term uType) ≔ Term eT ⇒ Term bool
definition PREDICATE (eT: Term uType) ≔ Term eT ⇒ Term bool
definition setof (eT: Term uType) ≔ Term eT ⇒ Term bool
definition SETOF (eT: Term uType) ≔ Term eT ⇒ Term bool

//
// exists1
//

//
// equality_props
//
rule if true &t  _ → &t
 and if false _ &f → &f

theorem if_same {T} b (x: Term T):
  Term (eq (if b x x) x)
proof
assume T
refine I.disjunction
  (λb, forall (λx, eq (if b x x) x))
  ?Cf[T] ?Ct[T]
assume x
simpl // FIXME [if] is not reduced at this step
admit

symbol reflexivity_of_equal T (x: Term T) : Term (eq x x)

symbol transitivity_of_equal T (x y z: Term T) :
  Term ((eq x y) ∧ (eq y z)) ⇒ Term (eq x z)

symbol symmetry_of_equal T (x y: Term T):
  Term (eq x y) ⇒ Term (eq y x)

//
// if_props
//
