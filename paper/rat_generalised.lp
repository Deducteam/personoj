require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.subtyping
require open personoj.encodings.bool_hol

set infix right 2 "⇒" ≔ imp
constant symbol rat: Set
rule Pull rat ↪ rat
rule pull rat $x ↪ $x
rule Dive rat _ ↪ true
rule dive rat $x _ ↪ $x

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
  Prf (∀ {nat} (λx, ¬ (z = (cast rat (EqvRefl nat) (s x) (λx, x)))))

rule z = z ↪ true
with cast rat _ (s $n) _ = cast rat _ (s $m) _
   ↪ @cast nat rat (EqvRefl nat) $n (λx, x) = @cast nat rat (EqvRefl nat) $m (λx, x)

theorem plus_closed_nat:
  Prf
  (∀ {nat} (λn,
   ∀ {nat} (λm,
   nat_p (cast rat (EqvRefl nat) n (λx, x) + cast rat (EqvRefl nat) m (λx, x)))))
proof
admit

// It’s just true ∧ plus_closed_nat n m
theorem tcc1:
  Prf
  (∀ {nat} (λn,
   ∀ {nat} (λm,
   true ∧ (λ_, nat_p (plus (cast rat (EqvRefl nat) n (λx, x))
                           (cast rat (EqvRefl nat) m (λx, x)))))))
proof
admit

set flag "print_implicits" on
compute λn: El nat, cast rat (EqvRefl rat) n (λx, x)

rule cast {nat} rat _ $n _ + z ↪ cast rat (EqvRefl rat) $n (λx, x)
with cast {nat} rat _ $n _ + cast rat _ (s $m) _
   ↪ cast {nat} rat (EqvRefl rat)
          (s (cast {rat} nat (EqvRefl rat)
                   (plus (cast {nat} rat (EqvRefl rat) $n (λx, x))
                         (cast {nat} rat (EqvRefl rat) $m (λx, x)))
                   _))
          (λx, x)

// compute λn: nat, cast rat (EqvRefl rat) n (λx, x)
