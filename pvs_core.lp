constant symbol Type : TYPE
constant symbol Prop : Type

injective symbol eta : Type ⇒ TYPE
injective symbol eps : eta Prop ⇒ TYPE

symbol arr : Type ⇒ Type ⇒ Type
set infix right 6 "~>" ≔ arr

rule eta (&A ~> &B) → eta &A ⇒ eta &B

// symbol psub : ∀ A : Type, eta (A ~> Prop) ⇒ Type
symbol psub : ∀ A : Type, (eta A ⇒ eta Prop) ⇒ Type

constant symbol cast : Type ⇒ Type
rule eta (cast (psub &A _)) → eta &A

symbol psubElim1 (A : Type) (P : eta (A ~> Prop)):
  eta (psub A P) ⇒ eta A

symbol imp : eta (Prop ~> Prop ~> Prop)
set infix right 6 "I>" ≔ imp

symbol impIntro : ∀ (p q : eta Prop), (eps p ⇒ eps q) ⇒ eps (p I> q)
symbol impElim : ∀ p q : eta Prop, eps (p I> q) ⇒ eps p ⇒ eps q

symbol all (A : Type): eta (A ~> Prop) ⇒ eta Prop
rule eta (all &A &P) → ∀ x : eta &A, eta (&P x)

// Forall intro
symbol allIntro (A: Type) (p: eta (A ~> Prop)) (x: eta A):
  eps (p x) ⇒ eps (all A p)
// Forall elim
symbol allElim (A: Type) (t: eta A) (p: eta (A ~> Prop)):
  eps (all A p) ⇒ eps (p t)


// Subtype elim 2
symbol psubElim (A: Type) (p: eta (A ~> Prop)) (t: eta (cast (psub A p))):
  eps (p t)
