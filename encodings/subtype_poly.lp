/// Sub-type polymorphism
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol
require open personoj.encodings.tuple
require open personoj.encodings.prenex

set declared "μ"
set declared "μ₀"
set declared "π"
set declared "⇑"
set declared "↓"

// Top type
symbol μ: Set → Set
rule μ (psub {$T} _) ↪ μ $T
with μ ($T ~> $U) ↪ $T ~> (μ $U)
with μ (tuple_t $T $U) ↪ tuple_t (μ $T) (μ $U)
with μ (arrd $b) ↪ arrd (λx, μ ($b x))
with μ (μ $T) ↪ μ $T // FIXME: can be proved

symbol π (T: Set): η (μ T) → Bool
rule π ($t ~> $u) ↪ λx: η $t → η (μ $u), ∀ (λy, π $u (x y))
with π (tuple_t $t $u)
   ↪ λx: η (tuple_t (μ $t) (μ $u)), π $t (p1 x) ∧ (λ_, π $u (p2 x))
with π (arrd $b)
   ↪ λx: η (arrd (λx, μ ($b x))), ∀ (λy, π ($b y) (x y))


/// Casting a symbol from its type to its top-type
symbol topcast {t: Set}: η t → η (μ t)
definition ⇑ {t} ≔ topcast {t}

symbol top_comp: Set → Set → Bool
set infix 6 "≃" ≔ top_comp

definition compatible (t u: Set) ≔ μ t ≃ μ u
set infix 6 "~" ≔ compatible

/// Casting between top-types ‘t’ and ‘u’ provided a proof that they are
/// equivalent.
symbol eqcast {t: Set} (u: Set): ε (t ~ u) → η (μ t) → η (μ u)
symbol eqcast_ {t: Set} (u: Set): η (μ t) → η (μ u) // Proof irrelevant
rule eqcast {$t} $u _ $m ↪ eqcast_ {$t} $u $m
rule eqcast_ {$t} $t $x ↪ $x

// Casting from/to maximal supertype
symbol downcast (t: Set) (x: η (μ t)): ε (π t x) → η t
definition ↓ t ≔ downcast t
symbol downcast_ (t: Set): η (μ t) → η t
rule downcast $t $x _ ↪ downcast_ $t $x

rule downcast_ $t (eqcast_ _ (topcast {$t} $x)) ↪ $x

rule π (psub {$t} $a)
   ↪ λx: η (μ $t), (π $t x) ∧ (λy: ε (π $t x), $a (↓ $t x y))

/// A term ‘x’ that has been cast up still validates the properties to be of its
/// former type.
symbol cstr_topcast_idem: ε (∀B (λt, ∀ {t} (λx, π t (⇑ x))))
// or as a rewrite-rule:
// rule ε (π _ (topcast _)) ↪ ε true

// The one true cast
definition cast {fr_t} to_t comp (m: η fr_t) cstr ≔
  ↓ to_t (eqcast {fr_t} to_t comp (⇑ m)) cstr

theorem comp_same_cstr_cast
        (fr to: Set)
        (comp: ε (μ fr ≃ μ to))
        (_: ε (eq {μ fr ~> bool}
                  (π fr)
                  (λx, π to (@eqcast fr to comp x))))
        (x: η fr)
      : ε (π to (@eqcast fr to comp (⇑ x)))
proof
  assume fr to comp eq_cstr x
  refine eq_cstr (λf, f (topcast x)) _
  refine cstr_topcast_idem fr x
qed

rule $t ≃ $t ↪ true
rule ($t1 ~> $u1) ≃ ($t2 ~> $u2)
   ↪ (μ $t1 ≃ μ $t2)
     ∧ (λh,
        (eq {μ $t1 ~> bool} (π $t1)
            (λx: η (μ $t1), π $t2 (@eqcast $t1 $t2 h x)))
        ∧ (λ_, $u1 ≃ $u2))
rule tuple_t $t1 $u1 ≃ tuple_t $t2 $u2
   ↪ $t1 ≃ $t2 ∧ (λ_, $u1 ≃ $u2)
with (arrd {$t1} $u1) ≃ (arrd {$t2} $u2)
   ↪ (μ $t1 ≃ μ $t2)
     ∧ (λh,
        (eq {μ $t1 ~> bool} (π $t1) (λx, π $t2 (@eqcast $t1 $t2 h x)))
        ∧ (λh', ∀
            (λx: η $t1,
             ($u1 x) ≃ ($u2 (cast {$t1} $t2 h x
                                            (comp_same_cstr_cast
                                             $t1 $t2 h h' x))))))
