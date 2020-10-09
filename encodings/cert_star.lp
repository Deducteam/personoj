require open personoj.encodings.lhol
require open personoj.encodings.bool_hol
require open personoj.encodings.tuple
require open personoj.encodings.pairs
require open personoj.encodings.prenex

constant symbol psub {t: Set}: (El t → Bool) → Set

// Returns the top type of any type
symbol pull: Set → Set
rule pull (psub {$T} _) ↪ pull $T
with pull (pull $T) ↪ pull $T

symbol tcc (t: Set) (_: El (pull t)): Bool

symbol fst {t: Set} (_: El t): El (pull t)

symbol pair' (t: Set) (_: El (pull t)): El t
definition pair t m (_: Prf (tcc t m)) ≔ pair' t m

rule tcc (psub {$t} $a) $x
   ↪ (tcc $t $x) ∧ (λy: Prf (tcc $t $x), $a (pair $t $x y))

rule pull bool ↪ bool
rule tcc bool $x ↪ true
rule fst {bool} $x ↪ $x
rule pair' bool $x _ ↪ $x

constant symbol equivalent: Set → Set → Bool
constant symbol eqv_refl (t: Set): Prf (equivalent t t)
definition compatible t u ≔ equivalent (pull t) (pull u)
set infix left 2 "~" ≔ compatible
definition comp_refl t ≔ eqv_refl (pull t)

protected symbol same' {t: Set} (u: Set) (_: El t): El u
definition same {t: Set} (u: Set) (_: Prf (equivalent t u)) (m: El t)
         ≔ same' u m

rule same' $t $t $m ↪ $m

definition cast {fr_t: Set} (to_t: Set) (comp: Prf (fr_t ~ to_t))
                (m: El fr_t)
                (hyp: Prf (tcc to_t (same (pull to_t) comp (fst m))))
         ≔ pair to_t (same (pull to_t) comp (fst m)) hyp
