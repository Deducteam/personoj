require open personoj.encodings.lhol
             personoj.encodings.pvs_cert
             personoj.encodings.logical
             personoj.encodings.equality;

constant symbol stack: Set;
constant symbol empty: El stack;

symbol nonempty_stack_p (s: El stack) ≔ ¬ (s = empty);
symbol nonempty_stack ≔ psub nonempty_stack_p;

constant symbol t: Set;
symbol push: El t → El stack → El nonempty_stack;
symbol pop: El nonempty_stack → El stack;
symbol top: El nonempty_stack → El t;

symbol push_top_pop (s: El stack) (hn: Prf (nonempty_stack_p s))
     : let sne ≔ pair s hn in
       Prf ((push (top sne) (pop sne)) = sne);

rule pop (push _ $s) ↪ $s;
rule top (push $x _) ↪ $x;
symbol pop_push (x: El t) (s: El stack): Prf ((pop (push x s)) = s);
symbol top_push (x: El t) (s: El stack): Prf ((top (push x s)) = x);

// The following theorem has unsolved meta variables
// theorem pop2push2 (x y: El t) (s: El stack)
//       : Prf (pop (pair (pop (push x (fst (push y s)))) _) = s)
// proof
// qed
