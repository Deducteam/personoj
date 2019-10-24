symbol const Type : TYPE
symbol const Prop : Type

symbol injective eta : Type ⇒ TYPE
symbol injective eps : eta Prop ⇒ TYPE

symbol arr : Type ⇒ Type ⇒ Type
set infix right 6 "~>" ≔ arr

rule eta (&A ~> &B) → eta &A ⇒ eta &B

symbol imp : eta (Prop ~> Prop ~> Prop)
set infix right 6 "I>" ≔ imp

symbol impIntro : ∀ (p q : eta Prop), (eps p ⇒ eps q) ⇒ eps (p I> q)
symbol impElim : ∀ p q : eta Prop, eps (p I> q) ⇒ eps p ⇒ eps q

symbol all : ∀ A : Type, eta (A ~> Prop) ⇒ eta Prop
rule eta (all &A &P) → ∀ x : eta &A, eta (&P x)

symbol allIntro : ∀ A : Type, ∀ p : eta (A ~> Prop), ∀ x : eta A, eps (p x)
       ⇒
       eps (all A p)
symbol allElim : ∀ A : Type, ∀ t : eta A, ∀ p : eta (A ~> Prop), eps (all A p)
       ⇒
       eps (p t)

symbol psub : ∀ A : Type, eta (A ~> Prop) ⇒ Type

symbol const cast : Type ⇒ Type
rule eta (cast (psub &A _)) → eta &A

symbol psubElim : ∀ A : Type, ∀ p : eta (A ~> Prop),
∀ t : eta (cast (psub A p)), eps (p t)
