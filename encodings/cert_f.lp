// PVS cert with functional PTS
constant symbol Sort : TYPE
constant symbol Univ : Sort ⇒ TYPE

constant symbol Kind : Sort
constant symbol Type : Sort
constant symbol Prop : Sort

// Axioms: (Prop, Type), (Type, Kind)
// Prop: Type
constant symbol uProp : Univ Type
// Type: Kind
constant symbol uType : Univ Kind

// Encoding of rules
// (Prop, Prop, Prop), (Type, Type, Type), (Type, Prop, Prop)
// Since the PTS is functional, [Rule s1 s2] rewrites the the 3rd element
// of the rule.
symbol Rule : Sort ⇒ Sort ⇒ Sort
rule Rule Prop Prop → Prop
 and Rule Type Type → Type
 and Rule Type Prop → Prop

// [Term s t] decodes term [t] of encoded sort [s]
injective symbol Term {s : Sort} : Univ s ⇒ TYPE

rule Term uProp → Univ Prop
 and Term uType → Univ Type

// [prod s1 s2 A B] encodes [Π x : (A: s1). (B: s2)]
symbol prod {s1 s2 : Sort} (A : Univ s1) :
  (Term A ⇒ Univ s2) ⇒ Univ (Rule s1 s2)

rule Term (@prod &s1 &s2 &A &B) → ∀ x : @Term &s1 &A, @Term &s2 (&B x)

// Predicate subtyping
// can be seen as a dependant pair type with
// - first element is a term of some type [A] and
// - second is a predicate on [A] verified by the first element.
symbol ePsub (A : Term uType) : (Term A ⇒ Term uProp) ⇒ Term uType

// Γ ⊢ M : { v : T | U }
// —————————————————————PROJl
//      Γ ⊢ fst(M) : T
symbol fst {T : Univ Type} (U : Term T ⇒ Univ Prop): Term (ePsub T U) ⇒ Term T

//  Γ ⊢ M : { v : T | U }
// ——————————————————————————PROJr
// Γ ⊢ snd(M) : U[v ≔ fst(M)]
symbol snd {T: Univ Type} (U: Term T ⇒ Univ Prop) (M: Term (ePsub T U)):
  Term (U (fst U M))

// An inhabitant of a predicate subtype, that is, a pair of
// an element and the proof that it satisfies the predicate
// Γ ⊢ M : T    Γ ⊢ N : U[v ≔ M]    Γ ⊢ { v : T | U }
// ——————————————————————————————————————————————————PAIR
//           Γ ⊢ ⟨M, N⟩ : {v : T | U}
symbol pair {T: Univ Type} (U: Term T ⇒ Univ Prop) (M: Term T):
  Term (U M) ⇒ Term (ePsub T U)

rule fst &U (pair &U &M _) → &M

// opair is a pair forgetting its snd argument
protected symbol opair (T: Univ Type) (U: Term T ⇒ Univ Prop) (M: Term T):
  Term (ePsub T U)

// Two pairs are convertible if their first element is
rule @pair &T &U &M _ → opair &T &U &M

// The subtype relation
symbol subtype: Term uType ⇒ Term uType ⇒ Term uProp
set infix left 6 "⊑" ≔ subtype

// [cast eA eB p t u] casts element [t] from type [eA] to type [eB] given
// the proof [p] that [eA] is a subtype of [eB]
symbol cast {eA: Term uType} (eB: Term uType):
  Term (eA ⊑ eB) ⇒ Term eA ⇒ Term eB
rule cast &eA &eA _ &x → &x
set declared "↑"
definition ↑ {eA: Term uType} ≔ @cast eA
