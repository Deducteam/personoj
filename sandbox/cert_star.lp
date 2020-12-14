require open personoj.encodings.lhol
require open personoj.encodings.prop_hol
require open personoj.encodings.tuple
require open personoj.encodings.pairs
require open personoj.encodings.prenex

constant symbol psub {t: Set}: (El t → Prop) → Set

// Returns the top type of any type
symbol pull: Set → Set
rule pull (psub {$T} _) ↪ pull $T
with pull (pull $T) ↪ pull $T

symbol tcc (t: Set) (_: El (pull t)): Prop

symbol fst {t: Set} (_: El t): El (pull t)

symbol pair' (t: Set) (_: El (pull t)): El t
definition pair t m (_: Prf (tcc t m)) ≔ pair' t m

rule fst (pair' _ $m) ↪ $m

rule tcc (psub {$t} $a) $x
   ↪ (tcc $t $x) ∧ (λy: Prf (tcc $t $x), $a (pair $t $x y))

rule pull prop ↪ prop
rule tcc prop _ ↪ true
rule fst {prop} $x ↪ $x
rule pair' prop $x _ ↪ $x

constant symbol equivalent: Set → Set → Prop
constant symbol eqv_refl (t: Set): Prf (equivalent t t)
definition compatible t u ≔ equivalent (pull t) (pull u)
set infix left 2 "~" ≔ compatible
definition comp_refl t ≔ eqv_refl (pull t)

protected symbol same' {t: Set} (u: Set) (_: El t): El u
rule same' $t $t $m ↪ $m
definition same {t: Set} (u: Set) (_: Prf (equivalent t u)) (m: El t)
         ≔ same' u m


definition cast {fr_t: Set} (to_t: Set) (comp: Prf (fr_t ~ to_t))
                (m: El fr_t)
                (hyp: Prf (tcc to_t (same (pull to_t) comp (fst m))))
         ≔ pair to_t (same (pull to_t) comp (fst m)) hyp
