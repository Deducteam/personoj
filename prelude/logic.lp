require open personoj.encodings.cert_f
personoj.adlib.bootstrap
require personoj.adlib.induction as I
require personoj.adlib.subtype   as S

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
definition neq {T: Term uType} (x y: Term T) ≔ bnot (eq x y)
set infix left 6 "/=" ≔ neq
set infix left 6 "≠" ≔ neq

//
// if_def
//
symbol if {T: Term uType}: Term uProp ⇒ Term T ⇒ Term T ⇒ Term T
// The reduction rules for if are in [equality_props]

//
// boolean_props
// Slightly modified from the prelude
constant symbol bool_exclusive: Term (neq false true)
constant symbol bool_inclusive A: Term ((eq A false) ∨ (eq A true))

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
definition xor (a b: Term bool) ≔ neq a b

theorem xor_def (a b: Term bool):
  Term (eq (xor a b) (if a (bnot b) b))
proof
refine I.disjunction
  (λa: Term bool, forall (λb, eq (xor a b) (if a (bnot b) b)))
  ?Cf ?Ct
refine I.disjunction
  (λ b, eq (xor false b) (if false (bnot b) b))
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
definition pred (eT: Univ Type) ≔ prod eT (λ_, bool)
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

constant symbol If_true {T} (x y: Term T): Term ((if true x y) = x)
constant symbol If_false {T} (x y: Term T): Term ((if false x y) = y)

theorem if_same {T} b (x: Term T):
  Term ((if b x x) = x)
proof
  assume T
  refine I.disjunction
    (λb, forall (λx, eq (if b x x) x))
    ?Cf[T] ?Ct[T]
  assume x
  refine If_false x x
  assume x
  refine If_true x x
qed

symbol reflexivity_of_equal T (x: Term T) : Term (eq x x)
// set builtin "refl" ≔ reflexivity_of_equal

symbol transitivity_of_equal T (x y z: Term T) :
  Term ((x = y) ∧ (y = z)) ⇒ Term (eq x z)

symbol symmetry_of_equal T (x y: Term T):
  Term (x = y) ⇒ Term (y = x)

//
// if_props
//
theorem lift_if1 (S T: Term uType) (a: Term bool) (x y: Term S)
  (f: Term S ⇒ Term T):
  Term ((f (if a x y)) = (if a (f x) (f y)))
proof
print
admit

theorem lift_if2 (S: Term uType) (a b c: Term bool) (x y: Term S):
  Term ((if (if a b c) x y) = (if a (if b x y) (if c x y)))
proof
admit
