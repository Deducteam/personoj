require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
rule ePsub &A _ ⊑ &A    → true
 and (ePsub &A &P) ⊑ (ePsub &B &Q) →
   let asb ≔ &A ⊑ &B in
   asb ∧
   (@forall &A
     (λx, imp (&P x)
              (&Q (↑ &B _ x))))
// FIXME the rewriting generates a new TCC in the cast

symbol refl eA: Term (eA ⊑ eA)
symbol carrier eA P: Term (ePsub eA P ⊑ eA)

symbol transitive (eA eB eC: Term uType):
  Term (eA ⊑ eB) ⇒ Term (eB ⊑ eC) ⇒ Term (eA ⊑ eC)

symbol destruct {eT eS: Term uType}
  (P: Term eT ⇒ Term bool) (Q: Term eS ⇒ Term bool)
  (psubt: Term (eT ⊑ eS)): // Proof of eT ⊑ eS
  ∀ x, Term (P x) ⇒ Term (Q (↑ eS psubt x)) ⇒
  Term (ePsub eT P ⊑ ePsub eS Q)
