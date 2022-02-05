require open personoj.lhol personoj.logical;

constant symbol psub [a: Set] (p: El a → Prop): Set;

// Computing supertype
symbol μ: Set → Set;
rule μ prop ↪ prop; // For all primitive type s, μ s = s
rule μ (@psub $a _) ↪ μ $a;
rule μ ($a ~> $b) ↪ $a ~> (μ $b);
//rule μ (σ ($a & $b)) ↪ σ (SVmap μ ($a & $b));
rule μ (μ $x) ↪ μ $x; // Required for open terms

symbol π (a: Set) (x: El (μ a)): Prop;

symbol elt [a: Set] (x: El a): El (μ a);
symbol witness (a: Set) (p: El a → Prop) (x: El (@psub a p)):
  Prf (π (@psub a p) (elt x));

symbol psi (a: Set) (p: El a → Prop) (x: El a)
           (_: Prf (π (@psub a p) (elt [a] x))):
  // Constructor for elements of predicate subtypes
  El (@psub a p);

constant symbol ≃ (a b: Set): Prop;
constant symbol ≃-refl (a: Set): Prf (≃ a a);
symbol ~ (a b: Set) ≔ ≃ (μ a) (μ b);
opaque symbol ~-refl (a: Set): Prf (~ a a) ≔ ≃-refl (μ a);
protected symbol cast'≃ (a b: Set) (x: El a): El b;
symbol cast≃ (a b: Set) (_: Prf (≃ a b)) (x: El a): El b ≔ cast'≃ a b x;
symbol cast~ (a b: Set) (ρ: Prf (~ a b)) (x: El a) ≔
  cast≃ (μ a) (μ b) ρ (elt [a] x);


symbol cast (fr: Set) (to: Set) (comp: Prf (~ fr to))
            (x: El fr) (_: Prf (π to (cast~ fr to comp x))):
  El to;

// Computing sub type constraints
rule π prop ↪ λ x, true; // For all primitive type
/*rule π (@psub $a $p) ↪
     λ x: El (μ $a), π $a x ∧ (λ hπx: Prf (π $a x), $p (cast (μ $a) $a (~-refl $a) x hπx)); */
