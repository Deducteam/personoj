require open personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.booleans
require personoj.encodings.deferred as Deferred
require open personoj.encodings.subtype //FIXME: why must it be the last import?

symbol if {U: Set} {V: Set} (S: Set)
: ε (U ⊑ S) ⇒ ε (V ⊑ S) ⇒ Bool ⇒ η U ⇒ η V ⇒ η S

symbol lif {U: Set} {V: Set} (S: Set)
: Deferred.p (U ⊑ S) ⇒ Deferred.p (V ⊑ S) ⇒ Bool ⇒ Deferred.t U ⇒ Deferred.t V ⇒ η S

// lazy if
set flag "print_implicits" on
rule lif _ &pr _ true &t _ → ↑ (Deferred.unquote_p &pr) (Deferred.unquote &t)
rule lif _ _ &pr false _ &f → ↑ (Deferred.unquote_p &pr) (Deferred.unquote &f)

// The if is lazy, that is, the expression [if true 2 (1/0)] must type check and
// return 2. For this, we use the [Deferred.t] datatype. But remember that [if]
// is typed by a super type of [2] and [1/0]. This super type is provided as
// argument, and the types of the expressions must be verified to be sub-types
// of this super-types. Since these arguments are deferred and can be ill-typed,
// we want to defer the verification of sub-type as well. Therefore, we
// introduce the [lifSet] type to type the [lif].
// [lifset &T &F &S p] reduces to the type of the function that maps a proof of
// &T is a sub-type of &S to &S if [p] is true and the function that maps a
// proof of &F is a sub-type of &S if [p] is false.
// The result of [lif S true e1 e2] is the function that maps the proof of
// sub-typing [Te1 ⊑ S] to the evaluation (or unquoting) of [e1].
type λ (T S: Set), ε (T ⊑ S) ⇒ η S
symbol lifSet (T U S: Set): Bool ⇒ TYPE
rule lifSet &U _ &S true → ε (&U ⊑ &S) ⇒ η &S
rule lifSet _ &F &S false → ε (&F ⊑ &S) ⇒ η &S

compute λ(U V S: Set), lifSet U V S true

symbol lifn {U: Set} {V: Set} (S: Set) (p: Bool)
: Deferred.t U ⇒ Deferred.t V ⇒ lifSet U V S p

rule lifn {&U} {&V} &S true &t &f → λh:ε (&U ⊑ &S), ↑ {&U} {&S} h (Deferred.unquote &t)
