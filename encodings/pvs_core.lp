constant symbol Type : TYPE
constant symbol Prop : Type

injective symbol El : Type → TYPE
injective symbol Prf : El Prop → TYPE

symbol arr : Type → Type → Type
set infix right 6 "~>" ≔ arr

rule El ($A ~> $B) ↪ El $A → El $B

// symbol psub : Π A : Type, El (A ~> Prop) → Type
symbol psub : Π A : Type, (El A → El Prop) → Type

constant symbol cast : Type → Type
rule El (cast (psub $A _)) ↪ El $A

symbol psubElim1 (A : Type) (P : El (A ~> Prop)):
  El (psub A P) → El A

symbol imp : El (Prop ~> Prop ~> Prop)
set infix right 6 "I>" ≔ imp

symbol impIntro : Π (p q : El Prop), (Prf p → Prf q) → Prf (p I> q)
symbol impElim : Π p q : El Prop, Prf (p I> q) → Prf p → Prf q

symbol all (A : Type): El (A ~> Prop) → El Prop
rule El (all $A $P) ↪ Π x : El $A, El ($P x)

// Forall intro
symbol allIntro (A: Type) (p: El (A ~> Prop)) (x: El A):
  Prf (p x) → Prf (all A p)
// Forall elim
symbol allElim (A: Type) (t: El A) (p: El (A ~> Prop)):
  Prf (all A p) → Prf (p t)

// Subtype elim 2
symbol psubElim (A: Type) (p: El (A ~> Prop)) (t: El (cast (psub A p))):
  Prf (p t)
