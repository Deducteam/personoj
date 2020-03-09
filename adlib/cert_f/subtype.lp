require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
// rule Term (ePsub &A _ ⊑ &A) → Term true
//  and Term ((ePsub &A &P) ⊑ (ePsub &B &Q)) →
//    ∀(pr: Term (&A ⊑ &B)) (x: Term &A),
//    Term (&P x) ⇒ Term (&Q (↑ &B pr x))
// FIXME 'File "src/basics.ml", line 116 character 36-42: Assertion failed'

symbol root: Term uType ⇒ Term uType
rule root (Psub &T _) → root &T
// TODO the [sub] rule needs to check that the two types have the same root,
// so the [eq] symbol must be loaded

symbol refl T: Term (T ⊑ T)
symbol restr T P: Term (Psub T P ⊑ T)

symbol trans (T U V: Term uType):
  Term (T ⊑ U) ⇒ Term (U ⊑ V) ⇒ Term (T ⊑ V)

// [sub {U} P T π ρ] proves that, given type [U], predicate from [U],
// type [T], proof [π] that [T] is a sub-type of [U] and proof [ρ] that
// any element of [T] verifies [P], [T] is a sub-type of [{x: U | P}].
symbol sub {U: Term uType} (P: Term U ⇒ Term bool) (T: Term uType)
  (pr: Term (T ⊑ U)):
  Term (forall (λx: Term T, P (↑ U pr x))) ⇒ Term (T ⊑ Psub U P)

// symbol sub {T S: Term uType}
//   (P: Term T ⇒ Term bool) (Q: Term S ⇒ Term bool)
//   (psubt: Term (T ⊑ S)): // Proof of T ⊑ S
//   Term (forall (λx, imp (P x) (Q (↑ S psubt x)))) ⇒
//   Term (Psub T P ⊑ Psub S Q)

// Transitivity of the cast
rule ↑ {&U} &V &pruv (↑ {&T} &U &prtu &x) →
  ↑ {&T} &V (trans &T &U &V &prtu &pruv) &x
