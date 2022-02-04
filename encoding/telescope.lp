/* Some kind of encoded telescope,
   see <https://doi.org/10.1016/0890-5401(91)90066-B>
   for more details on telescopes.

   Telescopes are sequences of possibly dependent types.

*/
require open personoj.lhol personoj.pvs_cert;
require personoj.extra.arity-tools as A;

/* NOTE: We postfix with a bang for operations on elements of type Set, so
   anything that ends with a bang deals with type codes. Dependent versions are
   prefixed with an ampersand & */

// The type of telescopes
constant symbol T : A.N → TYPE;
injective symbol &cons! [n: A.N] (x: Set): (El x → T n) → T (A.s n);
injective symbol cons! [n: A.N]: Set → T n → T (A.s n);
constant symbol nil!: T A.z;

// An example of a telescope: [n: nat][m: {k: nat | k < n}]
assert (nat: Set) (<: El nat → El nat → Prop) ⊢
  &cons! nat (λ n, cons! (psub (λ k, < k n)) nil!):
  T A.two;

// Exhibit non dependency
rule &cons! $x (λ _, $y.[]) ↪ cons! $x $y.[];

/* [code t] creates the code of the telescope */
constant symbol code [n: A.N]: T n → Set;

/* [nil] is the empty telescope. */
constant symbol nil : El (code nil!);
/* [cons x t] adds element [x] to telescope [t]. */
injective symbol &cons [n: A.N] [X: Set] [b: El X → T n] (x: El X):
  El (code (b x)) → El (code (&cons! X b));
injective symbol cons [n: A.N] [X: Set] [Q: T n] (x: El X) (q: El (code Q)):
  El (code (cons! X Q));

// Exhibit non dependency
rule @&cons _ _ (λ x, $b.[]) $x $q ↪ @cons _ _ $b.[] $x $q;

assert (nat: Set) (<: El nat → El nat → Prop)
       (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  &cons un (cons (pair zer h) nil)
  : El (code (&cons! nat (λ n, cons! (psub (λ k, < k n)) nil!)));

/// Accessors

symbol car! [n: A.N] (_: T n): Set;
rule car! (cons! $X _) ↪ $X
with car! (&cons! $X _) ↪ $X;

symbol cdr! [n: A.N] (tt: T (A.s n)) (arg: El (code tt)): T n;
rule cdr! (cons! _ $Q) _ ↪ $Q
with cdr! (&cons! _ $Q) (&cons $x _) ↪ $Q $x;

symbol car [n: A.N] [tt: T n] (_: El (code tt)): El (car! tt);
rule car (cons $x _) ↪ $x
with car (&cons $x _) ↪ $x;

symbol cdr [n: A.N] [tt: T (A.s n)] (arg: El (code tt)): El (code (cdr! tt arg));
rule cdr (cons _ $q) ↪ $q
with cdr (&cons _ $q) ↪ $q;

// Surjective pairing

rule cons! (car! $x) (cdr! $x _) ↪ $x;
rule cons (car $x) (cdr $x) ↪ $x;

/// Matching

/* [mkarr tele ret] creates the type [tele₀ ~> tele₁ ~> ... ~> ret]. */
injective symbol mkarr [n: A.N]: T n → Set → Set;
rule mkarr nil! $Ret ↪ $Ret
with mkarr (&cons! $X $Q) $Ret ↪ arrd $X (λ x, (mkarr ($Q x) $Ret))
with mkarr (cons! $X $Q) $Ret ↪ arr $X (mkarr $Q $Ret);

// mkarr [n: nat][m: [k:nat|k<n]] nat = (n: nat) -> [k:nat|k < n] -> nat
assert (nat: Set) (<: El nat → El nat → Prop) ⊢
  let tt ≔ &cons! nat (λ n, cons! (psub (λ k, < k n)) nil!) in
  mkarr tt nat ≡ arrd nat (λ n, arr (psub (λ k, < k n)) nat);

symbol match [l: A.N] [ret: Set] [tt: T l] (arg: El (code tt)):
  El (mkarr tt ret) → El ret;
rule match nil $e ↪ $e
with match (&cons $x $y) $f ↪ match $y ($f $x)
with match (cons $x $y) $f ↪ match $y ($f $x);

// [mkarr* tele] creates the type [tele₀ ~> tele₁ ~> ... ~> Set]. */
injective symbol mkarr* [n: A.N]: T n → TYPE;
rule mkarr* nil! ↪ Set
with mkarr* (&cons! $X $Q) ↪ Π x: El $X, mkarr* ($Q x)
with mkarr* (cons! $X $Q) ↪ El $X → mkarr* $Q;

symbol match* [l: A.N] [tt: T l] (arg: El (code tt)): mkarr* tt → Set;
rule match* nil $e ↪ $e
with match* (&cons $x $q) $f ↪ match* $q ($f $x)
with match* (cons $x $q) $f ↪ match* $q ($f $x);

/* match (un <zer, h>) (λ x1 x2, x1) = un */
assert (nat: Set) (<: El nat → El nat → Prop) (zer: El nat)
       (un: El nat) (h: Prf (< zer un)) ⊢
  let tt ≔ &cons! nat (λ n, cons! (psub (λ k, < k n)) nil!) in
  match [_] [_] [tt] (&cons un (cons (pair [nat] [λ k, < k un] zer h) nil))
        (λ x1 _, x1)
  ≡ un;

/// Projections

/* [nth! n tt arg] returns the [n]th type of telescope [tt], with types being
   successively substituted with [arg] (starting at 0). */
symbol nth! [l: A.N] (_: A.N) (tt: T l) (_: El (code tt)): Set;
rule nth! A.z (&cons! $X _) _ ↪ $X
with nth! A.z (cons! $X _) _ ↪ $X
with nth! (A.s $n) (&cons! _ $Tl) (&cons $X $Y) ↪ nth! $n ($Tl $X) $Y
with nth! (A.s $n) (cons! _ $Tl) (cons _ $Y) ↪ nth! $n $Tl $Y;

/* nth! (un <zero, zero < un) = un */
assert (nat: Set) (<: El nat → El nat → Prop) (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  let tt ≔ &cons! nat (λ n, &cons! (psub (λ k, < k n)) (λ _, nil!)) in
  nth! A.z tt (&cons un (&cons (pair [nat] [λ k, < k un] zer h) nil)) ≡ nat;

/* [nth n arg] returns the [nth] element of vector [arg] (starting at 0). */
symbol nth [l: A.N] [tt: T l] (n: A.N) (arg: El (code tt)):
  El (nth! n tt arg);
rule nth A.z (&cons $x _) ↪ $x
with nth A.z (cons $x _) ↪ $x
with nth (A.s $n) (&cons _ $y) ↪ nth $n $y
with nth (A.s $n) (cons _ $y) ↪ nth $n $y;

/* nth 0 (un <zer, h>) = un */
assert (nat: Set) (<: El nat → El nat → Prop)
       (zer: El nat) (un: El nat) (h: Prf (< zer un)) ⊢
  let tt ≔ &cons! nat (λ n, &cons! (psub (λ k, < k n)) (λ _, nil!)) in
  nth [_] [tt] A.z (&cons un (&cons (pair [nat] [λ k, < k un] zer h) nil)) ≡ un;

// Convenience

symbol &double! (a: Set) (b: El a → Set): T A.two ≔
  &cons! a (λ x, cons! (b x) nil!);
symbol double! (a b: Set): T A.two ≔ cons! a (cons! b nil!);

symbol &double [a: Set] [b: El a → Set] (x: El a) (y : El (b x)) : El (code (&double! a b))
  ≔ &cons x (cons y nil);
symbol double [a: Set] [b: Set] (x: El a) (y: El b) : El (code (double! a b))
  ≔ cons x (cons y nil);


/// Coercions

require open personoj.coercions;

/* All implicit arguments of the RHS must be explicited. Otherwise, they are
   considered as extra goals. This is due to the fact that there is no type
   checking of coercions yet. */

// Non dependent coercion
coercion #c (El (code [A.s $l] (cons! $a $v))) $arg (El (code (cons! $b $w))) ↪
  @cons $l $b $w
     (#c (El $a) (@car (A.s $l) (@cons! $l $a $v) $arg) (El $b))
     (#c (El (@code $l $v)) (@cdr $l (@cons! $l $a $v) $arg) (El (@code $l $w)));

assert (a: Set) (p: El a → Prop) (x: El (psub p)) ⊢ double x x : El (code (double! a a));

// Dependent coercion
coercion #c (El (code [A.s $l] (&cons! $a $v))) $arg (El (code (&cons! $b $w))) ↪
  let old-car : El $a ≔ @car (A.s $l) (&cons! $a $v) $arg in
  let new-car : El $b ≔ #c (El $a) (@car (A.s $l) (@&cons! $l $a $v) $arg) (El $b) in
  @&cons $l $b $w new-car
     (#c (El (@code $l ($v old-car))) (@cdr $l (@&cons! $l $a $v) $arg) (El (@code $l $w)));

assert (nat: Set) (<: El (nat ~> nat ~> prop))
       (n: El nat) (ln: El (psub (λ k, < k n))) ⊢
  &double n ln : El (code (&double! nat nat));

// Going from non dependent to dependent
coercion #c (El (code [A.s $l] (cons! $a $v))) $arg (El (code (&cons! $b $w))) ↪
  let new-car : El $b ≔
    #c (El $a) (@car (A.s $l) (@cons! $l $a $v) $arg) (El $b)
  in
  @&cons $l $b $w new-car
         (#c (El (@code $l $v))
             (@cdr $l (@cons! $l $a $v) $arg)
             (El (@code $l ($w new-car))));

assert
  // Example of coercion from non dependent to dependent telescope:
  // the function [f] has a dependent telescope type, but a non dependent one is
  // provided
  (nat: Set) (<: El (nat ~> nat ~> prop)) (n m: El nat) (h: Prf (< n m))
  (f: El ((code (&double! nat (λ k, (psub [nat] (λ i, < k i))))) ~> nat)) ⊢
  f (double n (pair [nat] [< n] m h)): El nat;

// Raises "Cannot solve < n m ≡ ?.[...] m" if the second argument of the pair is
// not provided
