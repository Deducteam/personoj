// Proof facilities
require open personoj.lhol personoj.pvs_cert personoj.logical;
require personoj.extra.arity-tools as A;

symbol and-i [P: Prop] [Q: Prop] (h: Prf P) (h': Prf Q) : Prf (P ∧ (λ _, Q)) ≔
  λ f, f h h';

// NOTE: it's tricky to use elaborate constructions such as the ones below
// because and* P reduces to P ∧ true, but Prf (P ∧ true) is not convertible with
// Prf P.

// Vectors of propositions
constant symbol PVec (n: A.N): TYPE;
constant symbol Prf* [n: A.N] (_: PVec n): TYPE;
constant symbol nil!: PVec A.z;
constant symbol cons! [n: A.N] (_: Prop) (_: PVec n): PVec (A.s n);

// Values of these vectors
constant symbol nil: Prf* nil!;
constant symbol cons [n: A.N] [P: Prop] [R: PVec n] (_: Prf P) (_: Prf* R): Prf* (cons! P R);

symbol and* [n: A.N] (_: PVec n): Prop;
rule and* (cons! $hd $tl) ↪ $hd ∧ (λ _, (and* $tl))
with and* nil! ↪ true;

//assert (P Q: Prop) ⊢ and* (cons! P (cons! Q nil!)) ≡ P ∧ (λ _, Q);

// The rule can also be written by deconstructing the type and using car and cdr
// on the proofs
symbol and-i* [n: A.N] [P: PVec n] (_: Prf* P): Prf (and* P);
rule and-i* nil ↪ λ x, x
with and-i* (cons $h $r) ↪ and-i $h (and-i* $r);

//assert (P Q: Prop) (h1: Prf P) (h2: Prf Q) ⊢ and-i* (cons h1 (cons h2 nil)): Prf (P ∧ (λ _, Q));
