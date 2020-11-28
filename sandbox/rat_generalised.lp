require open personoj.encodings.lhol
require open personoj.encodings.bool_hol
require open personoj.encodings.cert_star

set infix right 2 "⇒" ≔ imp
constant symbol rat: Set
rule pull rat ↪ rat
rule fst {rat} $x ↪ $x
rule tcc rat _ ↪ true
rule pair' rat $x ↪ $x

constant symbol z: El rat
symbol eq: El (rat ~> rat ~> bool)
set infix 3 "=" ≔ eq

symbol nat_p : El (rat ~> bool)
definition nat ≔ psub nat_p

constant symbol z_nat : Prf (nat_p z)
constant symbol s: El (nat ~> nat)
symbol plus: El (rat ~> rat ~> rat)
set infix left 4 "+" ≔ plus

constant symbol s_not_z:
  Prf (∀ {nat} (λx, ¬ (z = (cast rat (comp_refl nat) (s x) (λx, x)))))

rule z = z ↪ true
with cast rat _ (s $n) _ = cast rat _ (s $m) _
   ↪ @cast nat rat (comp_refl nat) $n (λx, x) = @cast nat rat (comp_refl nat) $m (λx, x)

theorem plus_closed_nat:
  Prf
  (∀ {nat} (λn,
   ∀ {nat} (λm,
   nat_p (cast rat (comp_refl nat) n (λx, x) + cast rat (comp_refl nat) m (λx, x)))))
proof
admit

// It’s just true ∧ plus_closed_nat n m
theorem tcc1:
  Prf
  (∀ {nat} (λn,
   ∀ {nat} (λm,
   true ∧ (λ_, nat_p
      (cast rat (comp_refl rat)
            (plus (cast rat (comp_refl nat) n (λx, x))
                  (cast rat (comp_refl nat) m (λx, x)))
            (λx, x))))))
proof
admit

set flag "print_implicits" on
compute λn: El nat, cast rat (comp_refl rat) n (λx, x)
type tcc1

rule cast {nat} rat _ $n _ + z ↪ cast rat (comp_refl rat) $n (λx, x)
with cast {nat} rat _ $n _ + cast rat _ (s $m) _
   ↪ cast {nat} rat (comp_refl rat)
          (s (cast {rat} nat (comp_refl rat)
                   (plus (cast {nat} rat (comp_refl rat) $n (λx, x))
                         (cast {nat} rat (comp_refl rat) $m (λx, x)))
                   (tcc1 $n $m)))
          (λx, x)

// compute λn: nat, cast rat (comp_refl rat) n (λx, x)
