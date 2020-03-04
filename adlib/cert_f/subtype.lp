require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
// rule Term (ePsub &A _ ⊑ &A) → Term true
//  and Term ((ePsub &A &P) ⊑ (ePsub &B &Q)) →
//    ∀(pr: Term (&A ⊑ &B)) (x: Term &A),
//    Term (&P x) ⇒ Term (&Q (↑ &B pr x))
// FIXME 'File "src/basics.ml", line 116 character 36-42: Assertion failed'

symbol refl T: Term (T ⊑ T)
symbol restr T P: Term (Psub T P ⊑ T)

symbol trans (T U V: Term uType):
  Term (T ⊑ U) ⇒ Term (U ⊑ V) ⇒ Term (T ⊑ V)

symbol sub {T S: Term uType}
  (P: Term T ⇒ Term bool) (Q: Term S ⇒ Term bool)
  (psubt: Term (T ⊑ S)): // Proof of eT ⊑ eS
  Term (forall (λx, imp (P x) (Q (↑ S psubt x)))) ⇒
  Term (Psub T P ⊑ Psub S Q)

// Transitivity of the cast
rule ↑ {&U} &V &pruv (↑ {&T} &U &prtu &x) →
  ↑ {&T} &V (trans &T &U &V &prtu &pruv) &x
