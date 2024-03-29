require open personoj.lhol;
require open personoj.pvs_cert;

// Prelude with some logics operator
symbol ⇒ P Q ≔ P ⇒ (λ _, Q); // Use a non dependent implication
notation ⇒ infix right 6;
symbol false ≔ ∀ [prop] (λ x, x);
symbol true ≔ false ⇒ false;
symbol ¬: Prop → Prop;
notation ¬ prefix 8;
rule Prf (¬ $x) ↪ Prf $x → Π(z: Prop), Prf z;

// Nat top type
constant symbol nat: Set;
// Presburger arithmetics
constant symbol s: El (nat ~> nat);
constant symbol z: El nat;
constant symbol eqnat: El (nat ~> nat ~> prop);
symbol +: El (nat ~> nat ~> nat);
notation + infix left 4;
constant symbol s_not_z: Prf (∀ (λ x, ¬ (eqnat z (s x))));
rule Prf (eqnat (s $n) (s $m)) ↪ Prf (eqnat $n $m) with Prf (eqnat z z) ↪ Prf true;
rule $n + z ↪ $n with $n + s $m ↪ s ($n + $m);
symbol nat_ind:
  Prf (∀ [nat ~> prop] (λ p, (p z) ⇒ (∀ (λ n, p n ⇒ p (s n))) ⇒ (∀ (λ n, p n))));

// System T
symbol rec_nat: El nat → El nat → (El nat → El nat → El nat) → El nat;
rule rec_nat z $t0 _ ↪ $t0;
rule rec_nat (s $u) $t0 $ts ↪ $ts $u (rec_nat $u $t0 $ts);

symbol mult a b ≔ rec_nat a z (λ _ r, b + r);

symbol * : El (nat ~> nat ~> nat);
notation * infix left 5;
rule z * _ ↪ z
with $n * (s $m) ↪ $n + ($n * $m)
with _ * z ↪ z; // (times_z_left)

// Declaration of a top type
constant symbol frac: Set;

symbol eqfrac: El (frac ~> frac ~> prop);

opaque symbol z_plus_n_n: Prf (∀ (λ n, eqnat (z + n) n)) ≔
begin
  assume n;
  refine nat_ind (λ n, eqnat (z + n) n) _ _ n
  { refine λ x: Prf false, x }
	{ assume _ Hn; apply Hn };
end;

// The following theorem allows to remove rule (times_z_left)
// but doing so would require to have eqnat transitivity, which requires some
// more work. So it is left for now.
opaque symbol n_times_z_z: Prf (∀ (λ n, eqnat (z * n) z)) ≔
begin
  assume n;
  refine nat_ind (λ n, eqnat (z * n) z) _ _ n
  { refine λ x: Prf false, x }
  { assume _ Hn; refine Hn };
end;

opaque symbol times_comm: Prf (∀ (λ a, ∀ (λ b, eqnat (a * b) (b * a)))) ≔
begin
admitted;

symbol nznat_p ≔ λ n, ¬ (eqnat z n);
symbol nznat ≔ psub nznat_p;

opaque symbol nzprod: Prf (∀ [nznat] (λ x, ∀ [nznat] (λ y, nznat_p (fst x * fst y)))) ≔
begin
admitted;

// Building rationals from natural numbers
symbol ÷: El (nat ~> nznat ~> frac);
notation ÷ infix left 6;
rule eqfrac ($a ÷ $b) ($c ÷ $d) ↪ eqnat ($a * (fst $d)) ((fst $b) * $c);

// rule Prf (nat_p ($n / pair $n _)) ↪ Prf true
// Non linear rules break confluence

symbol times_frac: El (frac ~> frac ~> frac);
rule times_frac ($a ÷ $b) ($c ÷ $d)
   ↪ let denom ≔ fst $b * (fst $d) in
     let prf ≔ nzprod $b $d in
     ($a * $c) ÷ (pair denom prf);


symbol one_nz ≔ pair [nat] [nznat_p] (s z) (s_not_z z);

opaque
symbol right_cancel:
  Prf (∀ (λ a, ∀ (λ b, eqfrac (times_frac (a ÷ b) (fst b ÷ one_nz)) (a ÷ one_nz))))
     ≔
begin
  assume x y;
  simplify;
  apply times_comm x (fst y);
end;
