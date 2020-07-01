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

constant symbol z: η rat

constant symbol eq: η (rat ~> rat ~> bool)
set infix 3 "=" ≔ eq

// Definition of a sub-type of ‘rat’
symbol nat_p: η (rat ~> bool) // Recogniser
definition nat ≔ psub nat_p

// z is a natural number
constant symbol z_nat: ε (nat_p z)

// Presburger arithmetics
constant symbol s: η (nat ~> nat)
symbol plus: η (rat ~> rat ~> rat)
set infix left 4 "+" ≔ plus

constant symbol s_not_z:
  ε (∀ {nat} (λx, ¬ (z = (cast rat (λx, x) (s x) (λx, x)))))

rule ε (z = z) ↪ ε true
rule ε ((cast rat _ (s $n) _) = (cast rat _ (s $m) _))
   ↪ ε ((cast rat (λx, x) $n (λx, x)) = (cast rat (λx, x) $m (λx, x)))

theorem plus_closed_nat:
  ε (∀ {nat} (λn, (∀ {nat} (λm, nat_p ((cast rat (λx, x) n (λx, x))
                                       + (cast rat (λx, x) m (λx, x)))))))
proof
admit

// It’s just true ∧ plus_closed_nat n m
theorem tcc1:
  ε (∀ {nat} (λn, (∀ {nat}
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

theorem tcc2: ε (true ∧ (λ_, nat_p z))
proof
admit

// symbol nat_ind:
//   ε (∀ {nat ~> bool}
//        (λp, (p (cast nat (λx, x) z _))
//             ⇒ (λ_, (∀ {nat} (λn, p n ⇒ (λ_, p (s n))))) ⇒ (λ_, (∀ {nat} (λn, p n)))))

// theorem z_plus_n_n: ε (∀ (λn, eqnat (z + n) n))
// proof
//   assume n
//   refine nat_ind (λn, eqnat (z + n) n) _ _ n
//   refine λx: ε false, x
//   assume n0 Hn
//   apply Hn
// qed

symbol times: η (rat ~> rat ~> rat)
set infix left 5 "*" ≔ times
rule z * _ ↪ z
with _ * z ↪ z // (times_z_left)

theorem times_comm: ε (∀ (λa, ∀ (λb, eq (a * b) (b * a))))
proof
admit

definition nznat_p (n: η nat) ≔ ¬ (eq z (cast rat (λx, x) n (λx, x)))
definition nznat ≔ psub nznat_p

symbol frac: η (nat ~> nznat ~> rat)
set infix left 6 "/" ≔ frac
rule ε (eq ($a / $b) ($c / $d))
   ↪ ε (eq (times (cast rat (λx, x) $a (λx, x))
                  (cast rat (λx, x) $d (λx, x)))
           (times (cast rat (λx, x) $b (λx, x))
                  (cast rat (λx, x) $c (λx, x))))

theorem prod_closed_nat:
  ε (∀ {nat} (λn, ∀ {nat} (λm, nat_p (times (cast rat (λx, x) n (λx, x))
                                            (cast rat (λx, x) m (λx, x))))))
proof
admit

theorem tcc4:
  ε (∀ {nznat} (λn, ∀ {nznat} (λm, and true
                                       (λ_, nat_p
                                            (times (cast {nznat} rat (λx, x) n (λx, x))
                                                   (cast {nznat} rat (λx, x) m (λx, x)))))))
proof
admit

theorem tcc3:
  ε (∀ {nznat}
       (λn, (∀ {nznat}
               (λm, true
                    ∧ (λ_, nznat_p
                           (cast nat (λx, x) (times (cast {nznat} rat (λx, x) n (λx, x))
                                                    (cast {nznat} rat (λx, x) m (λx, x)))
                                 (tcc4 n m)))))))
proof
admit

theorem tcc5:
  ε (∀ {nat} (λn, ∀ {nat} (λm, and true
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

theorem tcc6: ε (and true (λ_, nat_p z))
proof
admit
definition one ≔ s (cast nat (λx, x) z tcc6)
// FIXME: same as above
// theorem tcc7: ε (and true
//                      (λ_, and (nat_p (cast rat (λx, x) one (λx, x)))
//                               (λ_, nznat_p one)) )
// proof
// admit
// definition one_nz ≔ cast nznat (λx, x) one tcc7
