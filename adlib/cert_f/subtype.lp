require open encodings.cert_f
adlib.cert_f.booleans

set flag "print_implicits" on
// rule Term (ePsub &A _ ⊑ &A) → Term true
//  and Term ((ePsub &A &P) ⊑ (ePsub &B &Q)) →
//    ∀(pr: Term (&A ⊑ &B)) (x: Term &A),
//    Term (&P x) ⇒ Term (&Q (↑ &B pr x))
// FIXME 'File "src/basics.ml", line 116 character 36-42: Assertion failed'

// [eqroot T U] reduces to [true] if [T] and [U] have the same root
symbol eqroot: Term uType ⇒ Term uType ⇒ Term uProp
rule eqroot &X &X → true // NOTE: non linear
 and eqroot (Psub {&T} _) &U → eqroot &T &U
 and eqroot &T (Psub {&U} _) → eqroot &T &U

symbol eqtype: Term uType ⇒ Term uType ⇒ Term uProp

symbol refl T: Term (T ⊑ T)
symbol restr T P: Term (Psub {T} P ⊑ T)

symbol trans (T U V: Term uType):
  Term (T ⊑ U) ⇒ Term (U ⊑ V) ⇒ Term (T ⊑ V)

// [sub {U} P T μ π ρ] proves that, given type [U], [P] predicate from [U],
// type [T],
// - proof [μ] that [T] has the same root as [U];
// - proof [π] that [T] is a sub-type of [U];
// - proof [ρ] that // any element of [T] verifies [P];
// [T] is a sub-type of [{x: U | P}].
symbol sub {U: Term uType} (P: Term U ⇒ Term bool) (T: Term uType)
  (_: Term (eqroot T U)) (pr: Term (T ⊑ U)):
  Term (forall (λx: Term T, P (↑ U pr x))) ⇒ Term (T ⊑ Psub P)

// symbol sub {T S: Term uType}
//   (P: Term T ⇒ Term bool) (Q: Term S ⇒ Term bool)
//   (psubt: Term (T ⊑ S)): // Proof of T ⊑ S
//   Term (forall (λx, imp (P x) (Q (↑ S psubt x)))) ⇒
//   Term (Psub T P ⊑ Psub S Q)

// Transitivity of the cast
rule ↑ {&U} &V &pruv (↑ {&T} &U &prtu &x) →
  ↑ {&T} &V (trans &T &U &V &prtu &pruv) &x
