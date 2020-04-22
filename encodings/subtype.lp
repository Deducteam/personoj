require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_hol

set declared "↑"
set declared "↓"
set declared "↕"
set declared "μ"
set declared "μ₀"
set declared "π"

symbol S_Refl (a: Set): ε (a ⊑ a)
symbol S_Trans (s t u: Set): ε (s ⊑ t) → ε (t ⊑ u) → ε (s ⊑ u)

symbol S_Restr {a: Set} (p: η a → Bool): ε (psub p ⊑ a)

symbol S_Arr (t u1 u2: Set): ε (u1 ⊑ u2) → ε ((t ~> u1) ⊑ (t ~> u2))
symbol S_Darr (d: Set) (r1: η d → Set) (r2: η d → Set)
     : ε (∀ (λx, (r1 x) ⊑ (r2 x))) → ε ((arrd r1) ⊑ (arrd r2))

// Maximal supertype
symbol μ: Set → Set
rule μ (psub {$T} _) ↪ μ $T
with μ ($T ~> $U) ↪ $T ~> (μ $U)
with μ (μ $T) ↪ μ $T // FIXME: can be proved

rule μ bool ↪ bool
// Direct super-type
symbol μ₀: Set → Set
rule μ₀ (psub {$T} _) ↪ μ₀ $T

compute λT: Set, μ (μ T)

// Constraints
symbol upcast {A: Set} {B: Set}: ε (A ⊑ B) → η A → η B
definition ↑ {A} {B} ≔ upcast {A} {B}
rule upcast {$t} {$t} _ $x ↪ $x

theorem sub_of_sup A: ε (A ⊑ μ A)
proof
admit

symbol π {T: Set}: η (μ T) → Bool
symbol downcast {A: Set} {B: Set} (_: ε (B ⊑ A))
                (a: η A) (_: ε (π {A} (↑ {A} {μ A} (sub_of_sup A) a)))
     : η B
definition ↓ {A} {B} ≔ downcast {A} {B}

set flag "print_implicits" on
rule π {$T ~> $U} ↪ λx: η $T → η (μ $U), ∀(λy, π {$U} (x y))
// rule π {psub {$T} $a} ↪ λx: η (μ $T), π {$T} x
// rule π {psub {$T} $a}
//    ↪ λx: η (μ $T), (π {$T} x) ∧ (λy: ε (π {$T} x), $a (↓ {μ $T} {$T} _ x y))
// FIXME: does not type check because μ (μ $T) is not reduced to μ $T

constant symbol compatible: Set → Set → Bool
set infix left 6 "≃" ≔ compatible
constant symbol compcast {t: Set} {u: Set}: ε (t ≃ u) → η t → η u
definition ↕ {t} {u} ≔ compcast {t} {u}

definition comp (t: Set) (u: Set) ≔ μ t ≃ μ u
set infix 6 "∼" ≔ comp

rule ε (bool ≃ bool) ↪ ε true

rule ε (($t1 ~> $u1) ≃ ($t2 ~> $u2))
   ↪ ε ((μ $t1 ≃ μ $t2)
        ∧ (λh,
           (eq {μ $t1 ~> bool} (π {$t1}) (λx: η (μ $t1), π {$t2} (↕ h x)))
               ∧ (λ_, $u1 ≃ $u2)))
