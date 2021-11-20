// Constructive propositional calculus
require open lpvs.encoding.mpl;

constant symbol top: Prop;
constant symbol bot: Prop;
constant symbol conj: Prop → Prop → Prop;
constant symbol disj: Prop → Prop → Prop;
constant symbol not: Prop → Prop;
constant symbol ex: Set → (Set → Prop) → Prop;

builtin "bot" ≔ bot;
builtin "top" ≔ top;
builtin "and" ≔ conj;
builtin "or" ≔ disj;
builtin "not" ≔ not;
