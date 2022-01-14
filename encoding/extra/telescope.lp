/* Some kind of encoded telescope,
   see <https://doi.org/10.1016/0890-5401(91)90066-B>
   for more details on telescopes. */
require open personoj.lhol personoj.pvs_cert;
require personoj.extra.arity-tools as A;

symbol N ≔ A.N;
symbol + ≔ A.+;

symbol +2 ≔ + A.two;
symbol +3 ≔ + A.three;

constant symbol T : A.N → TYPE;
constant symbol cons [n: N] (x: Set): (El x → T n) → T (A.s n);
constant symbol nil: T A.z;

// An example of a telescope: [n: nat][m: [k: nat | k < n]]
assert (nat: Set) (<: El nat → El nat → Prop) ⊢
  cons nat (λ n, cons (psub (λ k, < k n)) (λ _, nil)):
  T A.two;

/* [τ t] creates the code of the telescope */
constant symbol τ [n: A.N]: T n → Set;

/* [mkarr tele ret] creates the type [tele₀ ~> tele₁ ~> ... ~> ret]. */
injective symbol mkarr [n: A.N]: T n → Set → Set;
rule mkarr nil $Ret ↪ $Ret
with mkarr (cons $X $Q) $Ret ↪ arrd $X (λ x, (mkarr ($Q x) $Ret));

// mkarr [n: nat][m: [k:nat|k<n]] nat = (n: nat) -> [k:nat|k < n] -> nat
assert (nat: Set) (<: El nat → El nat → Prop) ⊢
  let tt ≔ cons nat (λ n, cons (psub (λ k, < k n)) (λ _, nil)) in
  mkarr tt nat ≡ arrd nat (λ n, arrd (psub (λ k, < k n)) (λ _, nat));

/* [arg0] is the empty telescope. */
constant symbol arg0 : El (τ nil);
/* [argS x t] adds element [x] to telescope [t]. */
constant symbol argS [n: N] [X: Set] [b: El X → T n] (x: El X):
  El (τ (b x)) → El (τ (cons X b));

assert (nat: Set) (<: El nat → El nat → Prop)
       (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  argS un (argS (pair zer h) arg0)
  :
  El (τ (cons nat (λ n, cons (psub (λ k, < k n)) (λ _, nil))));

/// Matching

symbol match [l: A.N] [tt: T l] (ret: Set) (arg: El (τ tt)):
  El (mkarr tt ret) → El ret;
rule match [A.z] [nil] $Ret arg0 $e ↪ $e
with match $Ret (argS $x $y) $f ↪ match $Ret $y ($f $x);

/* match (un <zer, h>) (λ x1 x2, x1) = un */
assert (nat: Set) (<: El nat → El nat → Prop) (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  let tt ≔ cons nat (λ n, cons (psub (λ k, < k n)) (λ _, nil)) in
  match [_] [tt] nat (argS un (argS (pair [nat] [λ k, < k un] zer h) arg0)) (λ x1 _, x1) ≡ un;

/// Projections

/* [nth n tt arg] returns the [n]th type of telescope [tt], with types being
   successively substituted with [arg] (starting at 0). */
symbol nth [l: A.N] (_: A.N) (tt: T l) (_: El (τ tt)): Set;
rule nth A.z (cons $X _) _ ↪ $X
with nth (A.s $n) (cons _ $Tl) (argS $X $Y) ↪ nth $n ($Tl $X) $Y;

/* nth (un <zero, zero < un) = un */
assert (nat: Set) (<: El nat → El nat → Prop) (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  let tt ≔ cons nat (λ n, cons (psub (λ k, < k n)) (λ _, nil)) in
  nth A.z tt (argS un (argS (pair [nat] [λ k, < k un] zer h) arg0)) ≡ nat;

/* [argn n arg] returns the [nth] element of vector [arg] (starting at 0). */
symbol argn [l: A.N] [tt: T l] (n: A.N) (arg: El (τ tt)):
  El (nth n tt arg);
rule argn A.z (argS $x _) ↪ $x
with argn (A.s $n) (argS $x $y) ↪ argn $n $y;

/* argn 0 (un <zer, h>) = un */
assert (nat: Set) (<: El nat → El nat → Prop)
       (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  let tt ≔ cons nat (λ n, cons (psub (λ k, < k n)) (λ _, nil)) in
  argn [_] [tt] A.z (argS un (argS (pair [nat] [λ k, < k un] zer h) arg0)) ≡ un;
