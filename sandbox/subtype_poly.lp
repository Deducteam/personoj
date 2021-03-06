/// Sub-type polymorphism
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.prop_hol
require open personoj.encodings.tuple
require open personoj.encodings.pairs
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
with μ (σ $T $U) ↪ σ (μ $T) (μ $U)
with μ (arrd $b) ↪ arrd (λx, μ ($b x))
with μ (μ $T) ↪ μ $T // FIXME: can be proved

symbol π (T: Set): El (μ T) → Prop
rule π ($t ~> $u) ↪ λx: El $t → El (μ $u), ∀ (λy, π $u (x y))
with π (σ $t $u)
   ↪ λx: El (σ (μ $t) (μ $u)), π $t (σfst x) ∧ (λ_, π $u (σsnd x))
with π (arrd $b)
   ↪ λx: El (arrd (λx, μ ($b x))), ∀ (λy, π ($b y) (x y))


/// Casting a symbol from its type to its top-type
symbol topcast {t: Set}: El t → El (μ t)
definition ⇑ {t} ≔ topcast {t}
rule topcast {psub {$t} _} $u ↪ topcast {$t} (fst $u)
// A topcast is a sequence of first projections
with topcast {$a ~> _} $f ↪ λ(x: El $a), topcast ($f x)
// TODO: add tuple topcast

symbol top_comp: Set → Set → Prop
set infix 6 "≃" ≔ top_comp

definition compatible (t u: Set) ≔ μ t ≃ μ u
set infix 6 "~" ≔ compatible

/// Casting between top-types ‘t’ and ‘u’ provided a proof that they are
/// equivalent.
symbol eqcast {t: Set} (u: Set): Prf (t ~ u) → El (μ t) → El (μ u)
symbol eqcast_ {t: Set} (u: Set): El (μ t) → El (μ u) // Proof irrelevant
rule eqcast {$t} $u _ $m ↪ eqcast_ {$t} $u $m
rule eqcast_ {$t} $t $x ↪ $x

// Casting from/to maximal supertype
symbol downcast (t: Set) (x: El (μ t)): Prf (π t x) → El t
definition ↓ t ≔ downcast t
symbol downcast_ (t: Set): El (μ t) → El t
rule downcast $t $x _ ↪ downcast_ $t $x

rule downcast_ $t (eqcast_ _ (topcast {$t} $x)) ↪ $x

rule π (psub {$t} $a)
   ↪ λx: El (μ $t), (π $t x) ∧ (λy: Prf (π $t x), $a (↓ $t x y))

/// A term ‘x’ that has been cast up still validates the properties to be of its
/// former type.
symbol cstr_topcast_idem: Prf (∀B (λt, ∀ {t} (λx, π t (⇑ x))))
// or as a rewrite-rule:
// rule Prf (π _ (topcast _)) ↪ Prf true

// The one true cast
definition cast {fr_t} to_t comp (m: El fr_t) cstr ≔
  ↓ to_t (eqcast {fr_t} to_t comp (⇑ m)) cstr

theorem comp_same_cstr_cast
        (fr to: Set)
        (comp: Prf (μ fr ≃ μ to))
        (_: Prf (eq {μ fr ~> prop}
                  (π fr)
                  (λx, π to (@eqcast fr to comp x))))
        (x: El fr)
      : Prf (π to (@eqcast fr to comp (⇑ x)))
proof
  assume fr to comp eq_cstr x
  refine eq_cstr (λf, f (topcast x)) _
  refine cstr_topcast_idem fr x
qed

rule $t ≃ $t ↪ true
rule ($t1 ~> $u1) ≃ ($t2 ~> $u2)
   ↪ (μ $t1 ≃ μ $t2)
     ∧ (λh,
        (eq {μ $t1 ~> prop} (π $t1)
            (λx: El (μ $t1), π $t2 (@eqcast $t1 $t2 h x)))
        ∧ (λ_, $u1 ≃ $u2))
rule σ $t1 $u1 ≃ σ $t2 $u2
   ↪ $t1 ≃ $t2 ∧ (λ_, $u1 ≃ $u2)
with (arrd {$t1} $u1) ≃ (arrd {$t2} $u2)
   ↪ (μ $t1 ≃ μ $t2)
     ∧ (λh,
        (eq {μ $t1 ~> prop} (π $t1) (λx, π $t2 (@eqcast $t1 $t2 h x)))
        ∧ (λh', ∀
            (λx: El $t1,
             ($u1 x) ≃ ($u2 (cast {$t1} $t2 h x
                                            (comp_same_cstr_cast
                                             $t1 $t2 h h' x))))))
