require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.equality
  personoj.encodings.prenex
  personoj.encodings.if
  personoj.adlib.booleans
  personoj.encodings.subtype

definition neq {T: Set} (x y: η T) ≔ bnot (eq x y)
set infix left 6 "/=" ≔ neq
set infix left 6 "≠" ≔ neq


//
// boolean_props
//
constant symbol bool_exclusive: ε (neq {bool} false true)
constant symbol bool_inclusive
: ε (forall {bool} (λx, ((eq {bool} x false) ∨ (eq {bool} x true))))

compute ε (forall {bool} (λx, ((eq {bool} x false) ∨ (eq {bool} x true))))

theorem excluded_middle: ε (forall {bool} (λx, x ∨ ¬ x))
proof
admit

//
// xor_def
//
definition xor (a b: η bool) ≔ neq {bool} a b
theorem xor_def
: ε (forall {bool}
     (λa,
      (forall {bool} (λb, eq {bool}
                             (xor a b)
                             (if {bool} {bool} bool
                              (S_Refl bool) (S_Refl bool)
                              a (bnot b) b)))))
proof
  simpl
admit

//
// Quantifier props
//
set declared "∃"
// Declared as a lemma in the prelude
definition ∃ {T: Set} (P: η T ⇒ η bool) ≔ ¬ (forall (λx, ¬ (P x)))

//
// Defined types
//
definition pred (T: Set) ≔ arrd {T} (λ_, bool)
definition PRED ≔ pred
definition predicate ≔ pred
definition PREDICATE ≔ pred
definition setof ≔ pred
definition SETOF ≔ pred

symbol reflexivity_of_equal: ε (forallp_bool (λT, forall {T} (λx, eq x x)))
set builtin "refl" ≔ reflexivity_of_equal

symbol transitivity_of_equal
: ε (forallp_bool
     (λT, forall
          {T} (λx, forall
                   {T} (λy, forall
                            {T} (λz, (imp ((x = y) ∧ (y = z)) (x = z)))))))

symbol symmetry_of_equal
: ε (forallp_bool
     (λT, forall
          {T} (λx, forall
                   {T} (λy, (imp (x = y) (y = x))))))

//
// if_props
//
theorem lift_if1
: ε (forallp_bool
     (λs,
      (forallp_bool
       (λt,
        (forall
         {bool} (λa,
                 forall
                 {s} (λx,
                      forall
                      {s} (λy,
                           forall
                           {s ~> t}
                           (λf, eq
                                (f (if s (S_Refl s) (S_Refl s) a x y))
                                (if t (S_Refl t) (S_Refl t) a (f x) (f y)))))))))))
proof
admit
