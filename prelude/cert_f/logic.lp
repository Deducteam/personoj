require open encodings.cert_f
require adlib.cert_f.induction as I

//
// Booleans
//
definition bool ≔ uProp
definition false: Term bool ≔ @prod Type Prop uProp (λ x, x)
definition true: Term bool ≔ @prod Prop Prop false (λ _, false)

definition imp (P Q: Term uProp) ≔ @prod Prop Prop P (λ_, Q)
definition forall {eT: Term uType} (P: Term eT ⇒ Term bool) ≔
  @prod _ _ eT P
// FIXME explicitness?

definition bnot (P: Term uProp) ≔ @prod Prop Prop P (λ _, false)
set prefix 5 "¬" ≔ bnot
definition band (P Q: Term uProp) ≔ bnot (imp P (bnot Q))
set infix 6 "∧" ≔ band
definition bor (P Q: Term uProp) ≔ imp (bnot P) Q
set infix 5 "∨" ≔ bor

definition biff (P Q: Term uProp) ≔ (imp P Q) ∧ (imp Q P)
set infix 7 "⇔" ≔ biff

definition when (P Q: Term uProp) ≔ imp Q P

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
theorem xor_def: ∀ (a b: Term bool),
  Term (@eq bool (xor a b) (@if bool a (bnot b) b))
proof
assume a
refine I.disjunction
  (λx: Term bool, @eq bool (xor a x) (@if bool a (bnot x) x))
  ?Cf[a] ?Ct[a]
// FIXME needs generalize?
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

//
// exists1
//

//
// equality_props
//
rule if true &t  _ → &t
 and if false _ &f → &f

theorem if_same T (b: Term uProp) (x: Term T):
  Term (eq (if b x x) x)
proof
assume T b x
simpl
admit // Needs case disjunciton on boolean

symbol reflexivity_of_equal T (x: Term T) : Term (eq x x)

symbol transitivity_of_equal T (x y z: Term T) :
  Term ((eq x y) ∧ (eq y z)) ⇒ Term (eq x z)

symbol symmetry_of_equal T (x y: Term T):
  Term (eq x y) ⇒ Term (eq y x)

//
// if_props
//
