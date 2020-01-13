require open pvs_core prelude.booleans prelude.equalities prelude.if_def

// true /= false
symbol bool_inclusive : eps (bnot (eq bool false true))

// A ∨ ¬ A
symbol bool_exclusive (A:eta bool):
    eps (bor (eq bool A false) (eq bool A true))

theorem excluded_middle : ∀ A : eta bool, eps (bor A (bnot A))
proof
admit
