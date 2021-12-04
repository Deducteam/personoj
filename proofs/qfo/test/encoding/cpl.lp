// Constructive propositional calculus
// TODO: define the connectives
require open lpvs.lhol;

constant symbol top: Prop;
constant symbol bot: Prop;
constant symbol conj: Prop → Prop → Prop;
constant symbol disj: Prop → Prop → Prop;
constant symbol not: Prop → Prop;
constant symbol ex: Set → (Set → Prop) → Prop;
constant symbol imp: Prop → Prop → Prop;

builtin "imp" ≔ imp;
builtin "bot" ≔ bot;
builtin "top" ≔ top;
builtin "and" ≔ conj;
builtin "or" ≔ disj;
builtin "not" ≔ not;
