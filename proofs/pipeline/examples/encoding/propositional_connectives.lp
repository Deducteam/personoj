// Klassische propositional logic
require open qfo.encoding.lhol;

constant symbol top: Prop;
constant symbol bot: Prop;
constant symbol imp: Prop → Prop → Prop;
constant symbol conj: Prop → Prop → Prop;
constant symbol disj: Prop → Prop → Prop;
constant symbol not: Prop → Prop;
constant symbol ex (a: Set): (El a → Prop) → Prop;
symbol all (a: Set): (El a → Prop) → Prop;

rule Prf (all $a $p) ↪ Π x: El $a, Prf ($p x);
// Allow to assume over prenex quantifications

//builtin "T" ≔ El;
//builtin "P" ≔ Prf;
builtin "imp" ≔ imp;
builtin "bot" ≔ bot;
builtin "top" ≔ top;
builtin "and" ≔ conj;
builtin "or" ≔ disj;
builtin "not" ≔ not;

//opaque symbol ok : Prf (∀ {prop} (λ (x: El prop), imp x x)) ≔ begin assume x; why3; end;
