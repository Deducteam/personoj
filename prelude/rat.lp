require open pvs_core booleans equalities notequal

constant symbol rat : Type
constant symbol zero : eta rat

symbol div : eta rat ⇒ eta rat ⇒ eta rat
symbol times : eta rat ⇒ eta rat ⇒ eta rat

set infix 5 "*" ≔ times
set infix 6 "/" ≔ div

symbol zero_times : ∀ x : eta rat, eps (eq rat zero (zero * x))
symbol left_cancellation : ∀ x y : eta rat, eps (eq rat (x * y / x) y)

symbol nonzero : psub rat (λ x : eta rat, neq rat zero x)
