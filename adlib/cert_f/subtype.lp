require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
rule Term (ePsub &A _ ⊑ &A) → Term bool
 and Term ((ePsub &A &P) ⊑ (ePsub &B &Q)) →
   ∀(pr: Term (&A ⊑ &B)) (x: Term &A),
   Term (&P x) ⇒ Term (&Q (↑ &B pr x))
// FIXME 'File "src/basics.ml", line 116 character 36-42: Assertion failed'

symbol refl eA: Term (eA ⊑ eA)
symbol carrier eA P: Term (ePsub eA P ⊑ eA)

symbol transitive (eA eB eC: Term uType):
  Term (eA ⊑ eB) ⇒ Term (eB ⊑ eC) ⇒ Term (eA ⊑ eC)

symbol destruct {eT eS: Term uType}
  (P: Term eT ⇒ Term bool) (Q: Term eS ⇒ Term bool)
  (psubt: Term (eT ⊑ eS)): // Proof of eT ⊑ eS
  ∀ x, Term (P x) ⇒ Term (Q (↑ eS psubt x)) ⇒
  Term (ePsub eT P ⊑ ePsub eS Q)
