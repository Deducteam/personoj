// λHOL encoding (à la PTS)
symbol Sort : TYPE

symbol Univ : Sort ⇒ TYPE

constant symbol Kind : Sort
constant symbol Prop : Sort
constant symbol Type : Sort

constant symbol True : TYPE
constant symbol I : True

symbol Axiom : Sort ⇒ Sort ⇒ TYPE
rule Axiom Prop Type → True
 and Axiom Type Kind → True

symbol Rule : Sort ⇒ Sort ⇒ Sort ⇒ TYPE
rule Rule Prop Prop Prop → True
 and Rule Type Type Type → True
 and Rule Type Prop Prop → True
 and Rule Type Prop Type → True  // Dependent pairs

injective symbol Term : ∀ s : Sort, Univ s ⇒ TYPE
constant symbol univ : ∀ s1 s2 : Sort, (Axiom s1 s2) ⇒ Univ s2
rule Term &s2 (univ &s1 &s2 I) → Univ &s1

symbol prod :
  ∀ s1 s2 s3 : Sort,
  ∀ A : Univ s1,
  (Rule s1 s2 s3) ⇒ (Term s1 A ⇒ Univ s2) ⇒ Univ s3

rule Term &s3 (prod &s1 &s2 &s3 &A I &B) →
     ∀ x : (Term &s1 &A), Term &s2 (&B x)

// Predicate subtyping

// Γ ⊢ T : Type    Γ, v : T ⊢ U : Prop
// ———————————————————————————————————SUBTYPE
//      Γ ⊢ { v : T | U } : Type
symbol psub :
  ∀ A : Univ Type,
  (Term Type A ⇒ Univ Prop) ⇒ Univ Type

set declared "πl"
set declared "πr"

// Γ ⊢ M : { v : T | U }
// —————————————————————PROJl
//      Γ ⊢ πl(M) : T
symbol πl :
       ∀ T : Univ Type,
       ∀ U : (Term Type T ⇒ Univ Prop),
       Term Type (psub T U) ⇒ Term Type T

//  Γ ⊢ M : { v : T | U }
// ————————————————————————PROJr
// Γ ⊢ πr(M) : U[v ≔ πl(M)]
symbol πr :
       ∀ T : Univ Type,
       ∀ U : (Term Type T ⇒ Univ Prop),
       ∀ M : Term Type (psub T U),
       Term Prop (U (πl T U M))

// An inhabitant of a predicate subtype, that is, a pair of
// an element and the proof that it satisfies the predicate
// Γ ⊢ M : T    Γ ⊢ N : U[v ≔ M]    Γ ⊢ { v : T | U }
// ——————————————————————————————————————————————————PAIR
//           Γ ⊢ ⟨M, N⟩ : {v : T | U}
symbol pair :
  ∀ T : Univ Type,
  ∀ U : (Term Type T ⇒ Univ Prop), // Predicate
  ∀ M : Term Type T,
  Term Prop (U M)                  // Proof of predicate
  ⇒ Term Type (psub T U)

rule πl &T &U (pair &T &U &M _) → &M
rule πr &T &U (pair &T &U _ &P) → &P
// Protected pair forgetting the proof that [M] verifies predicate [U]
protected symbol pair' :
  ∀ T : Univ Type,
  ∀ U : (Term Type T ⇒ Univ Prop),
  Term Type T ⇒ Term Type (psub T U)

rule pair &T &U &M _ → pair' &T &U &M
