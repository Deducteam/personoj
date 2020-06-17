/// Sub-type polymorphism
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol
require open personoj.encodings.tuple
require open personoj.encodings.prenex

set declared "μ"
set declared "μ₀"
set declared "π"
set declared "↑"
set declared "↓"

// Maximal supertype
symbol μ: Set → Set
rule μ (psub {$T} _) ↪ μ $T
with μ ($T ~> $U) ↪ $T ~> (μ $U)
with μ (tuple_t $T $U) ↪ tuple_t (μ $T) (μ $U)
with μ (arrd $b) ↪ arrd (λx, μ ($b x))
with μ (μ $T) ↪ μ $T // FIXME: can be proved

symbol π (T: Set): η (μ T) → Bool

// Casting from/to maximal supertype
constant symbol maxcast {t: Set}: η t → η (μ t)
constant symbol downcast {t: Set} (x: η (μ t)): ε (π (μ t) x) → η t
definition ↑ {t} ≔ maxcast {t}
definition ↓ {t} ≔ downcast {t}

rule π ($t ~> $u) ↪ λx: η $t → η (μ $u), ∀ (λy, π $u (x y))
with π (tuple_t $t $u)
   ↪ λx: η (tuple_t (μ $t) (μ $u)), π $t (p1 x) ∧ (λ_, π $u (p2 x))
with π (psub {$t} $a)
   ↪ λx: η (μ $t), (π (μ $t) x) ∧ (λy: ε (π (μ $t) x), $a (↓ x y))
with π (arrd $b)
   ↪ λx: η (arrd (λx, μ ($b x))), ∀ (λy, π (μ ($b y)) (x y))

/// A term ‘x’ that has been cast up still verifies the properties to be of its
/// former type.
symbol cstr_maxcast_idem: ε (∀B (λt, ∀ {t} (λx, π t (maxcast x))))
// or as a rewrite-rule:
// rule ε (π _ (maxcast _)) ↪ ε true

private constant symbol max_eq: Set → Set → Bool
set infix 6 "≃" ≔ max_eq

symbol eqcast {fr: Set} {to: Set}: ε (fr ≃ to) → η fr → η to

definition compatible (t u: Set) ≔ μ t ≃ μ u
set infix 6 "~" ≔ compatible

// The one true cast
symbol cast {fr_t: Set} (to_t: Set) (comp: ε (fr_t ~ to_t)) (m: η fr_t):
  ε (π to_t (eqcast comp (maxcast m))) → η to_t

/// Proof irrelevance
protected symbol _cast {fr_t: Set} (to_t: Set): η fr_t → η to_t
rule cast {$f_t} $t_t _ $x _ ↪ _cast {$f_t} $t_t $x
rule _cast {$t} $t $x ↪ $x

theorem comp_same_cstr_cast
        (fr to: Set)
        (comp: ε (μ fr ≃ μ to))
        (_: ε (eq {μ fr ~> bool}
                  (π fr)
                  (λx, π to (eqcast comp x))))
        (x: η fr)
      : ε (π to (eqcast comp (maxcast x)))
proof
  assume fr to comp eq_cstr x
  refine eq_cstr (λf, f (maxcast x)) _
  refine cstr_maxcast_idem fr x
qed

rule ε ($t ≃ $t) ↪ ε true
rule ε (($t1 ~> $u1) ≃ ($t2 ~> $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {μ $t1 ~> bool} (π $t1) (λx: η (μ $t1), π $t2 (eqcast h x)))
           ∧ (λ_, $u1 ≃ $u2)))
rule ε (tuple_t $t1 $u1 ≃ tuple_t $t2 $u2)
   ↪ ε ($t1 ≃ $t2 ∧ (λ_, $u1 ≃ $u2))
with ε ((arrd {$t1} $u1) ≃ (arrd {$t2} $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {μ $t1 ~> bool} (π $t1) (λx, π $t2 (eqcast h x)))
           ∧ (λh', ∀
               (λx: η $t1,
                ($u1 x) ≃ ($u2 (cast {$t1} $t2 h x
                                               (comp_same_cstr_cast
                                                $t1 $t2 h h' x)))))))
