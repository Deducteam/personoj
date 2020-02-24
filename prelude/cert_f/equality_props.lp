require open encodings.cert_f
prelude.cert_f.booleans
prelude.cert_f.equalities
prelude.cert_f.if_def

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
