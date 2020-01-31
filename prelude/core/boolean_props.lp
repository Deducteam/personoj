require open encodings.pvs_core
prelude.core.booleans prelude.core.equalities prelude.core.if_def

// true /= false
symbol bool_inclusive : eps (bnot (eq bool false true))

// A ∨ ¬ A
symbol bool_exclusive (A:eta bool):
    eps (bor (eq bool A false) (eq bool A true))

theorem excluded_middle : ∀ A : eta bool, eps (bor A (bnot A))
proof
admit
