// Klassische propositional logic
// TODO: define connectives using constructive ones
require open lpvs.encoding.lhol;
require lpvs.encoding.cpl as C;

constant symbol top: Prop;
constant symbol bot: Prop;
constant symbol imp: Prop → Prop → Prop;
constant symbol conj: Prop → Prop → Prop;
constant symbol disj: Prop → Prop → Prop;
constant symbol not: Prop → Prop;
constant symbol ex: Set → (Set → Prop) → Prop;
constant symbol all: Set → (Set → Prop) → Prop;

builtin "imp" ≔ imp;
builtin "bot" ≔ bot;
builtin "top" ≔ top;
builtin "and" ≔ conj;
builtin "or" ≔ disj;
builtin "not" ≔ not;
