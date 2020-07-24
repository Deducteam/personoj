require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.subtype_poly
require open personoj.encodings.bool_hol

set infix right 2 "⇒" ≔ imp

// Declaration of a top type
constant symbol rat: Set
rule π rat ↪ λ_, true
rule μ rat ↪ rat
// rule topcast {rat} $x ↪ $x
// rule downcast_ rat $e ↪ $e

constant symbol z: El rat

constant symbol eq: El (rat ~> rat ~> bool)
set infix 3 "=" ≔ eq

// Definition of a sub-type of ‘rat’
symbol nat_p: El (rat ~> bool) // Recogniser
definition nat ≔ psub nat_p

// z is a natural number
constant symbol z_nat: Prf (nat_p z)

// Presburger arithmetics
constant symbol s: El (nat ~> nat)
symbol plus: El (rat ~> rat ~> rat)
set infix left 4 "+" ≔ plus

constant symbol s_not_z:
  Prf (∀ {nat} (λx, ¬ (z = (cast rat (λx, x) (s x) (λx, x)))))

rule Prf (z = z) ↪ Prf true
rule Prf ((cast rat _ (s $n) _) = (cast rat _ (s $m) _))
   ↪ Prf ((cast rat (λx, x) $n (λx, x)) = (cast rat (λx, x) $m (λx, x)))

theorem plus_closed_nat:
  Prf (∀ {nat} (λn, (∀ {nat} (λm, nat_p ((cast rat (λx, x) n (λx, x))
                                       + (cast rat (λx, x) m (λx, x)))))))
proof
admit

// It’s just true ∧ plus_closed_nat n m
theorem tcc1:
  Prf (∀ {nat} (λn, (∀ {nat}
                     (λm, true
                          ∧ (λ_, nat_p (plus (cast rat (λx, x) n (λx, x))
                                             (cast rat (λx, x) m (λx, x))))))))
proof
admit

set flag "print_implicits" on
rule (cast {nat} rat _ $n _) + z ↪ cast rat (λx, x) $n (λx, x)
with (cast {nat} rat _ $n _) + (cast {nat} rat _ (s $m) _)
   ↪ cast {nat} rat (λx, x)
          (s (cast {rat} nat (λx, x)
                   (plus (cast {nat} rat (λx, x) $n (λx, x))
                         (cast {nat} rat (λx, x) $m (λx, x)))
              (tcc1 $n $m)))
          (λx, x)

theorem tcc2: Prf (true ∧ (λ_, nat_p z))
proof
admit

// symbol nat_ind:
//   Prf (∀ {nat ~> bool}
//        (λp, (p (cast nat (λx, x) z _))
//             ⇒ (λ_, (∀ {nat} (λn, p n ⇒ (λ_, p (s n))))) ⇒ (λ_, (∀ {nat} (λn, p n)))))

// theorem z_plus_n_n: Prf (∀ (λn, eqnat (z + n) n))
// proof
//   assume n
//   refine nat_ind (λn, eqnat (z + n) n) _ _ n
//   refine λx: Prf false, x
//   assume n0 Hn
//   apply Hn
// qed

symbol times: El (rat ~> rat ~> rat)
set infix left 5 "*" ≔ times
rule z * _ ↪ z
with _ * z ↪ z // (times_z_left)

theorem times_comm: Prf (∀ (λa, ∀ (λb, eq (a * b) (b * a))))
proof
admit

definition nznat_p (n: El nat) ≔ ¬ (eq z (cast rat (λx, x) n (λx, x)))
definition nznat ≔ psub nznat_p

symbol frac: El (nat ~> nznat ~> rat)
set infix left 6 "/" ≔ frac
rule Prf (eq ($a / $b) ($c / $d))
   ↪ Prf (eq (times (cast rat (λx, x) $a (λx, x))
                  (cast rat (λx, x) $d (λx, x)))
           (times (cast rat (λx, x) $b (λx, x))
                  (cast rat (λx, x) $c (λx, x))))

theorem prod_closed_nat:
  Prf (∀ {nat} (λn, ∀ {nat} (λm, nat_p (times (cast rat (λx, x) n (λx, x))
                                            (cast rat (λx, x) m (λx, x))))))
proof
admit

theorem tcc4:
  Prf (∀ {nznat} (λn, ∀ {nznat} (λm, and true
                                       (λ_, nat_p
                                            (times (cast {nznat} rat (λx, x) n (λx, x))
                                                   (cast {nznat} rat (λx, x) m (λx, x)))))))
proof
admit

theorem tcc3:
  Prf (∀ {nznat}
       (λn, (∀ {nznat}
               (λm, true
                    ∧ (λ_, nznat_p
                           (cast nat (λx, x) (times (cast {nznat} rat (λx, x) n (λx, x))
                                                    (cast {nznat} rat (λx, x) m (λx, x)))
                                 (tcc4 n m)))))))
proof
admit

theorem tcc5:
  Prf (∀ {nat} (λn, ∀ {nat} (λm, and true
                                   (λ_,
                                    nat_p (times (cast {nat} rat (λx, x) n (λx, x))
                                                 (cast {nat} rat (λx, x) m (λx ,x)))))))
proof
admit


// FIXME: patterns not in normal forms do not behave correctly:
// the pattern is matched as-is against a normal form, and they thus do not
// match.
// It happens here for the ‘cast’ terms.
// rule times ($a / $b) ($c / $d)
//    ↪ frac (cast nat (λx, x)
//                 (times (cast rat (λx, x) $a (λx, x))
//                        (cast rat (λx, x) $c (λx, x)))
//                 (tcc5 $a $c))
//           (cast {rat} nznat (λx, x)
//                 (times (cast {nznat} rat (λx, x) $b (λx, x))
//                        (cast {nznat} rat (λx, x) $d (λx, x)))
//                 (tcc3 $b $d))

theorem tcc6: Prf (and true (λ_, nat_p z))
proof
admit
definition one ≔ s (cast nat (λx, x) z tcc6)
// FIXME: same as above
// theorem tcc7: Prf (and true
//                      (λ_, and (nat_p (cast rat (λx, x) one (λx, x)))
//                               (λ_, nznat_p one)) )
// proof
// admit
// definition one_nz ≔ cast nznat (λx, x) one tcc7
