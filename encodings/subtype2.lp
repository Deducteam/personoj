require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

set declared "μ"
set declared "μ₀"
set declared "π"
set declared "↑"
set declared "↓"

// Maximal supertype
constant symbol μ: Set → Set
rule η (μ (psub {$T} _)) ↪ η (μ $T)
with η (μ ($T ~> $U)) ↪ η ($T ~> (μ $U))
with η (μ (arrd $b)) ↪ η (arrd (λx, μ ($b x) ))
with η (μ (μ $T)) ↪ η (μ $T) // FIXME: can be proved

symbol π {T: Set}: η (μ T) → Bool

// Casting from/to maximal supertype
constant symbol maxcast {t: Set}: η t → η (μ t)
constant symbol downcast {t: Set} (x: η (μ t)): ε (π x) → η t
definition ↑ {t} ≔ maxcast {t}
definition ↓ {t} ≔ downcast {t}

rule ε (π {$t ~> $u}) ↪ ε (λx: η $t → η (μ $u), forall (λy, π (x y)))
with ε (π {psub {$t} $a}) ↪ ε (λx: η (μ $t), (π x) ∧ (λy: ε (π x), $a (↓ x y)))
with ε (π {arrd $b}) ↪ ε (λx: η (arrd (λx, μ ($b x))), forall (λy, π (x y)))

rule ε (π (maxcast _)) ↪ ε true // FIXME: to  be justified

// FIXME: is protected needed?
protected constant symbol max_eq: Set → Set → Bool
set infix 6 "≃" ≔ max_eq

protected symbol eqcast {fr: Set} {to: Set}: ε (fr ≃ to) → η fr → η to

definition compatible (t u: Set) ≔ μ t ≃ μ u
set infix 6 "~" ≔ compatible

// The one true cast
injective symbol cast {fr: Set} {to: Set} (comp: ε (fr ~ to)) (x: η fr):
// Proof that [x] verifies the constraints of [to]
  let xtop : η (μ fr) ≔ maxcast x in
  let x_to_top: η (μ to) ≔ eqcast comp xtop in
  ε (π x_to_top) → η to

rule cast {$t} {$t} _ $x _ ↪ $x

theorem comp_same_cstr_cast
        (fr to: Set)
        (comp: ε (μ fr ≃ μ to))
        (_: ε (eq {fr ~> bool}
                  (λx: η fr, π {fr} (maxcast x))
                  (λx: η fr, π {to} (eqcast comp (maxcast x)))))
        (x: η fr)
      : let xtop ≔ maxcast x in
        let x_to_top ≔ eqcast comp xtop in
        ε (π x_to_top)
proof
  assume fr to comp eq_cstr x
  refine eq_cstr (λf, f x) _
  refine λx: ε false, x
qed


rule ε (($t1 ~> $u1) ≃ ($t2 ~> $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {μ $t1 ~> bool} (π {$t1}) (λx: η (μ $t1), π {$t2} (eqcast h x)))
          ∧ (λ_, $u1 ≃ $u2)))
with ε ((arrd {$t1} $u1) ≃ (arrd {$t2} $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {$t1 ~> bool}
               (λx, π (maxcast x))
               (λx, π (eqcast h (maxcast x))))
           ∧ (λh', forall
               (λx: η $t1,
                ($u1 x) ≃ ($u2 (cast {$t1} {$t2} h x
                                     (comp_same_cstr_cast $t1 $t2 h h' x)))))))
