require open pvs_cert explicit.pts_bindings explicit.booleans explicit.equalities explicit.notequal

constant symbol Nat : Term Kind uType
symbol Nz : Term Type Nat
symbol Nsuc : Term Type Nat ⇒ Term Type Nat

constant symbol rat : Term Kind uType
constant symbol Qz : Term Type rat

definition notzero ≔ psub uProp (λ x : Term Type uProp, x)

symbol frac : Term Type Nat ⇒ ∀ m : Term Type notzero, Term Type rat

symbol times : Term Type rat ⇒ Term Type rat ⇒ Term Type rat

rule times (frac &A (pair &B &P)) (frac &C (pair &D &Q)) →
  frac (Nat_times &A &C) (pair (Nat_times &B &D) (Nat_prod_not_zero &B &D &P &Q))
