// Klassische propositional logic
// TODO: define connectives using constructive ones
require open qfo.encoding.lhol;
require qfo.encoding.cpl as C;

constant symbol top: Prop;
constant symbol bot: Prop;
constant symbol imp: Prop → Prop → Prop;
constant symbol conj: Prop → Prop → Prop;
constant symbol disj: Prop → Prop → Prop;
constant symbol not: Prop → Prop;
constant symbol ex: Set → (Set → Prop) → Prop;
constant symbol all: Set → (Set → Prop) → Prop;

//builtin "T" ≔ El;
//builtin "P" ≔ Prf;
builtin "imp" ≔ imp;
builtin "bot" ≔ bot;
builtin "top" ≔ top;
builtin "and" ≔ conj;
builtin "or" ≔ disj;
builtin "not" ≔ not;

opaque symbol ok : Prf (∀ {prop} (λ (x: El prop), imp x x)) ≔ begin assume x; why3; end;
