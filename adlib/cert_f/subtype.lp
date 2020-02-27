require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
rule ePsub &A _ ⊑ &A    → true
 and (ePsub &A &P) ⊑ (ePsub &B &Q) →
   let asb ≔ &A ⊑ &B in
   asb ∧
   (@forall &A
     (λx, imp (&P x)
              (&Q (cast &B _ x))))

symbol refl eA: Term (eA ⊑ eA)
symbol carrier eA P: Term (ePsub eA P ⊑ eA)

symbol transitive (eA eB eC: Term uType):
  Term (eA ⊑ eB) ⇒ Term (eB ⊑ eC) ⇒ Term (eA ⊑ eC)

symbol restriction (c d: Term uType)
  (p: Term c ⇒ Term bool) (q: Term d ⇒ Term bool):
  ∀ x: Term c, Term (p x) ⇒ Term (q (↑ d _ x))

symbol destruct (c1 c2: Term uType) p1 p2
  (ps1: Term (ePsub c1 p1)) (ps2: Term (ePsub c2 p2)):
    Term (c1 ⊑ c2) ⇒ ∀x: Term c1, Term (p1 x) ⇒
    Term (p2 (↑ c2 _ x)) ⇒
    Term (ps1 <| ps2)
