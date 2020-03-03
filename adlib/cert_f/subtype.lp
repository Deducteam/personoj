require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
// rule Term (ePsub &A _ ⊑ &A) → Term true
//  and Term ((ePsub &A &P) ⊑ (ePsub &B &Q)) →
//    ∀(pr: Term (&A ⊑ &B)) (x: Term &A),
//    Term (&P x) ⇒ Term (&Q (↑ &B pr x))
// FIXME 'File "src/basics.ml", line 116 character 36-42: Assertion failed'

symbol refl eA: Term (eA ⊑ eA)
symbol restr eA P: Term (ePsub eA P ⊑ eA)

symbol trans (eA eB eC: Term uType):
  Term (eA ⊑ eB) ⇒ Term (eB ⊑ eC) ⇒ Term (eA ⊑ eC)

symbol sub {eT eS: Term uType}
  (P: Term eT ⇒ Term bool) (Q: Term eS ⇒ Term bool)
  (psubt: Term (eT ⊑ eS)): // Proof of eT ⊑ eS
  Term (forall (λx, imp (P x) (Q (↑ eS psubt x)))) ⇒
  Term (ePsub eT P ⊑ ePsub eS Q)
