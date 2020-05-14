require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

set declared "μ"
set declared "μ₀"
set declared "π"
set declared "↑"
set declared "↓"

// Maximal supertype
symbol μ: Set → Set
rule μ (psub {$T} _) ↪ μ $T
with μ ($T ~> $U) ↪ $T ~> (μ $U)
with μ (arrd $b) ↪ arrd (λx, μ ($b x) )
with μ (μ $T) ↪ μ $T // FIXME: can be proved

set unif_rule μ $x ≡ μ $y ↪ $x ≡ $y
// set unif_rule μ $x ≡ $y ↪ μ $x ≡ μ $y

symbol π {T: Set}: η (μ T) → Bool

// Casting from/to maximal supertype
constant symbol maxcast {t: Set}: η t → η (μ t)
constant symbol downcast {t: Set} (x: η (μ t)): ε (π {μ t} x) → η t
definition ↑ {t} ≔ maxcast {t}
definition ↓ {t} ≔ downcast {t}

// rule η (maxcast (maxcast $t)) ↪ η (maxcast $t)

rule π {$t ~> $u} ↪ λx: η $t → η (μ $u), ∀ (λy, π (x y))
with π {psub {$t} $a}
   ↪ λx: η (μ $t), (π {μ $t} x) ∧ (λy: ε (π {μ $t} x), $a (↓ x y))
with π {arrd $b}
   ↪ λx: η (arrd (λx, μ ($b x))), ∀ (λy, π {μ ($b y)} (x y))

rule ε (π (maxcast _)) ↪ ε true // FIXME: to  be justified

// FIXME: is protected needed?
protected constant symbol max_eq: Set → Set → Bool
set infix 6 "≃" ≔ max_eq

injective symbol eqcast {fr: Set} {to: Set}: ε (fr ≃ to) → η fr → η to

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
        (_: ε (eq {μ fr ~> bool}
                  (π {fr})
                  (λx, π {to} (eqcast comp x))))
        (x: η fr)
      : let xtop ≔ maxcast x in
        let x_to_top ≔ eqcast comp xtop in
        ε (π x_to_top)
proof
  assume fr to comp eq_cstr x
  refine eq_cstr (λf, f (maxcast x)) _
  refine λx: ε false, x
qed

rule ε ($t ≃ $t) ↪ ε true
rule ε (($t1 ~> $u1) ≃ ($t2 ~> $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {μ $t1 ~> bool} (π {$t1}) (λx: η (μ $t1), π {$t2} (eqcast h x)))
          ∧ (λ_, $u1 ≃ $u2)))
with ε ((arrd {$t1} $u1) ≃ (arrd {$t2} $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {μ $t1 ~> bool} π (λx, π (eqcast h x)))
           ∧ (λh', ∀
               (λx: η $t1,
                ($u1 x) ≃ ($u2 (cast {$t1} {$t2} h x
                                     (comp_same_cstr_cast $t1 $t2 h h' x)))))))
