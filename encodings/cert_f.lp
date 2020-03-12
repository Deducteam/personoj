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
injective symbol Term {s: Sort}: Univ s ⇒ TYPE
rule Term uProp → Univ Prop
 and Term uType → Univ Type

// [prod s1 s2 A B] encodes [Π x : (A: s1). (B: s2)]
symbol prod {sA: Sort} {sB: Sort} (A: Univ sA):
  (Term A ⇒ Univ sB) ⇒ Univ (Rule sA sB)

rule Term (prod {&sA} {&sB} &A &B) → ∀x: Term {&sA} &A, Term {&sB} (&B x)

// Predicate subtyping
// can be seen as a dependant pair type with
// - first element is a term of some type [A] and
// - second is a predicate on [A] verified by the first element.
constant symbol Psub {T: Term uType}: (Term T ⇒ Term uProp) ⇒ Term uType

// Γ ⊢ M : { v : T | P }
// —————————————————————PROJl
//      Γ ⊢ fst(M) : T
symbol fst {T: Univ Type} (P: Term T ⇒ Univ Prop): Term (Psub P) ⇒ Term T

//  Γ ⊢ M : { v : T | P }
// ——————————————————————————PROJr
// Γ ⊢ snd(M) : P[v ≔ fst(M)]
constant symbol snd {T: Univ Type} {P: Term T ⇒ Univ Prop}
  (M: Term (Psub P)):
  Term (P (fst P M))

// An inhabitant of a predicate subtype, that is, a pair of
// an element and the proof that it satisfies the predicate
// Γ ⊢ M : T    Γ ⊢ N : P[v ≔ M]    Γ ⊢ { v : T | P }
// ——————————————————————————————————————————————————PAIR
//           Γ ⊢ ⟨M, N⟩ : {v : T | P}
symbol pair {T: Univ Type} (P: Term T ⇒ Univ Prop) (M: Term T):
  Term (P M) ⇒ Term (Psub P)

rule fst _ (pair _ &M _) → &M

// opair is a pair forgetting its snd argument
protected symbol opair (T: Univ Type) (P: Term T ⇒ Univ Prop) (M: Term T):
  Term (Psub P)

// Two pairs are convertible if their first element is
rule pair {&T} &P &M _ → opair &T &P &M

// The subtype relation
symbol subtype: Term uType ⇒ Term uType ⇒ Term uProp
set infix left 6 "⊑" ≔ subtype

// [↑ {T} U p t] casts element [t] from type [T] to type [U] given
// the proof [p] that [T] is a subtype of [U]
set declared "↑"
symbol ↑ {T: Term uType} (U: Term uType): Term (T ⊑ U) ⇒ Term T ⇒ Term U
rule ↑ {&T} &T _ &x → &x // Identity cast
// NOTE: a cast from a type [{x: A | P}] to type [A] is a [fst],
 and ↑ {Psub {&T} &P} &T _ → fst &P
// and a "downcast" is the pair constructor
set declared "↓"

// Auxiliary definitions

// [↓ {T} P t π] down casts term [t] to type [{x: T | P(x)}] given the
// proof [π] that [t] verifies predicate [P].
definition ↓ {T} ≔ pair {T}
// NOTE: we can only down-cast from a type to its direct sub-type

definition arrow {sA} {sB} (A: Univ sA) (B: Univ sB) ≔ prod A (λ_, B)
set infix left 5 "~>" ≔ arrow

// Builtins
definition T ≔ Term {Type}
set builtin "T" ≔ T

definition P ≔ Term {Prop}
set builtin "P" ≔ P
