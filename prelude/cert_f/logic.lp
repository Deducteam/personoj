require open encodings.cert_f
adlib.cert_f.booleans
require adlib.cert_f.induction as I
require adlib.cert_f.subtype as S

//
// Booleans
// In [adlib.cert_f.booleans]

//
// Equalities
//
symbol eq {T: Term uType}: Term T ⇒ Term T ⇒ Term uProp
set infix 5 "=" ≔ eq

// NOTE not in the prelude
constant symbol cast_trans (A B C: Term uType) (prab: Term (A ⊑ B)) (prbc: Term (B ⊑ C))
  (x: Term A):
  Term (eq (↑ {B} C prbc (↑ {A} B prab x))
  (↑ {A} C (S.trans A B C prab prbc) x))

//
// Notequal
//
definition neq {T: Term uType} (x y: Term T) ≔ bnot (eq x y)
set infix left 6 "/=" ≔ neq
set infix left 6 "≠" ≔ neq

//
// if_def
//
symbol if {T: Term uType} : Term uProp ⇒ Term T ⇒ Term T ⇒ Term T
// The reduction rules for if are in [equality_props]

//
// boolean_props
// Slightly modified from the prelude
// FIXME explicitness required
constant symbol bool_exclusive: Term (@neq bool false true)
constant symbol bool_inclusive A: Term ((@eq bool A false) ∨ (@eq bool A true))

theorem excluded_middle (A: Term bool): Term (A ∨ ¬ A)
proof
  refine I.disjunction
    (λA, A ∨ (¬ A))
    ?Ct ?Cf
  assume F1 F2
  refine F2
  assume F1 F2
  refine F1 F2
qed

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
¬ (forall (λx, ¬ (P x)))

//
// Defined types
//
definition pred (eT: Univ Type) ≔ @prod Type Type eT (λ_, bool)
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
