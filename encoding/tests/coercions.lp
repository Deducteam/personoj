require open personoj.lhol
             personoj.pvs_cert
             personoj.logical
             personoj.coercions;

constant symbol nat : Set;
constant symbol z : El nat;
constant symbol even : El (nat ~> prop);
constant symbol z_even : Prf (even z);

// Coercion "psub-fst" (without transitivity)
compute (λ x: El nat, x) (@pair nat even z z_even);

// Coercion "psub-pair" (without transitivity)
compute (λ x: El (psub even), x) z;
