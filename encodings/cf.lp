// PVS cert with functional PTS
symbol Sort : TYPE
symbol Univ : Sort ⇒ TYPE

constant symbol Kind : Sort
constant symbol Type : Sort
constant symbol Prop : Sort

// Axioms: (Prop, Type), (Type, Kind)
// Prop: Type
symbol uProp : Univ Type
// Type: Kind
symbol uType : Univ Kind

// Encoding of rules
// (Prop, Prop, Prop), (Type, Type, Type), (Type, Prop, Prop)
// Since the PTS is functional, [Rule s1 s2] rewrites the the 3rd element
// of the rule.
symbol Rule : Sort ⇒ Sort ⇒ Sort
rule Rule Prop Prop → Prop
 and Rule Type Type → Type
 and Rule Type Prop → Prop

// [Term s t] decodes term [t] of encoded sort [s]
symbol Term {s : Sort} : Univ s ⇒ TYPE

rule Term uProp → Univ Prop
 and Term uType → Univ Type

// [prod s1 s2 A B] encodes [Π x : (A: s1). (B x: s2)]
symbol prod {s1 s2 : Sort} (A : Univ s1) :
  (@Term s1 A ⇒ Univ s2) ⇒ Univ (Rule s1 s2)

rule Term _ (prod &s1 &s2 &A &B) → ∀ x : Term &A, Term (&B x)

// Predicate subtyping
// can be seen as a dependant pair type with
// - first element is a term of some type [A] and
// - second is a predicate on [A] verified by the first element.
symbol psub (A : Univ Type) : (@Term Type A ⇒ Univ Prop) ⇒ Univ Type

// Γ ⊢ M : { v : T | U }
// —————————————————————PROJl
//      Γ ⊢ fst(M) : T
symbol fst {T : Univ Type} {U : @Term Type T ⇒ Univ Prop}:
  @Term Type (psub T U) ⇒ @Term Type T

//  Γ ⊢ M : { v : T | U }
// ——————————————————————————PROJr
// Γ ⊢ snd(M) : U[v ≔ fst(M)]
symbol snd (T: Univ Type) (U: @Term Type T ⇒ Univ Prop)
  (M: @Term Type (psub T U)): @Term Prop (U (@fst T U M))

// An inhabitant of a predicate subtype, that is, a pair of
// an element and the proof that it satisfies the predicate
// Γ ⊢ M : T    Γ ⊢ N : U[v ≔ M]    Γ ⊢ { v : T | U }
// ——————————————————————————————————————————————————PAIR
//           Γ ⊢ ⟨M, N⟩ : {v : T | U}
symbol pair {T: Univ Type} (U: @Term Type T ⇒ Univ Prop) (M: @Term Type T):
  @Term Prop (U M) ⇒ @Term Type (psub T U)

rule @fst &T &U (@pair &T &U &M _) → &M
rule @snd &T &U (@pair &T &U _ &P) → &P
