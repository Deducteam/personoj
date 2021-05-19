constant symbol Type : TYPE;
constant symbol Prop : TYPE;

injective symbol El : Type → TYPE;
injective symbol Prf : Prop → TYPE;

constant symbol prop : Type;
rule El prop ↪ Prop;

symbol arr : Type → Type → Type;

rule El (arr $A $B) ↪ (El $A) → (El $B);

// symbol psub : Π A : Type, El (A ~> Prop) → Type
symbol psub : Π A : Type, (El A → Prop) → Type;

symbol psubElim1 (A : Type) (P : El A → Prop):
  El (psub A P) → El A;

symbol imp : Prop → Prop → Prop;

symbol impIntro : Π (p q : Prop), (Prf p → Prf q) → Prf (imp p q);
symbol impElim : Π (p q : Prop), Prf (imp p q) → Prf p → Prf q;

symbol all (A : Type): (El A → Prop) → Prop;
rule Prf (all $A $P) ↪ Π x : El $A, Prf ($P x);

// Forall intro
symbol allIntro (A: Type) (p: El A → Prop) (x: El A):
  Prf (p x) → Prf (all A p);
// Forall elim
symbol allElim (A: Type) (t: El A) (p: El A → Prop):
  Prf (all A p) → Prf (p t);

// Subtype elim 2
//symbol psubElim (A: Type) (p: El A → Prop) (t: El (psub A p)):
//  Prf (p t);
