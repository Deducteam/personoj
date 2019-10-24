symbol Sort : TYPE

symbol Univ : Sort ⇒ TYPE

symbol const Kind : Sort
symbol const Prop : Sort
symbol const Type : Sort

symbol const True : TYPE
symbol const I : True

symbol Axiom : Sort ⇒ Sort ⇒ TYPE
rule Axiom Prop Type → True
 and Axiom Type Kind → True

symbol const univ : ∀ s1 s2 : Sort, (Axiom s1 s2) ⇒ Univ s2

symbol Rule : Sort ⇒ Sort ⇒ Sort ⇒ TYPE
rule Rule Prop Prop Prop → True
 and Rule Type Type Type → True
 and Rule Type Prop Prop → True
 and Rule Type Prop Type → True  // Dependent pairs

symbol injective Term : ∀ s : Sort, Univ s ⇒ TYPE
rule Term &s2 (univ &s1 &s2 I) → Univ &s1

symbol prod :
       ∀ s1 s2 s3 : Sort,
       ∀ A : Univ s1,
       (Rule s1 s2 s3) ⇒ (Term s1 A ⇒ Univ s2) ⇒ Univ s3

rule Term &s3 (prod &s1 &s2 &s3 &A I &B) →
     ∀ x : (Term &s1 &A), Term &s2 (&B x)


symbol psub :
       ∀ A : Univ Type,
       (Term Type A ⇒ Univ Prop) ⇒ Univ Type

symbol proj_l :
       ∀ T : Univ Type,
       ∀ U : (Term Type T ⇒ Univ Prop),
       Term Type (psub T U) ⇒ Term Type T

symbol proj_r :
       ∀ T : Univ Type,
       ∀ U : (Term Type T ⇒ Univ Prop),
       ∀ M : Term Type (psub T U),
       Term Prop (U (proj_l T U M))

symbol pair :
       ∀ T : Univ Type,
       ∀ U : (Term Type T ⇒ Univ Prop),
       Term Type (psub T U)
