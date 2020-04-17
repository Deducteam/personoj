require open encodings.pvs_core
prelude.core.booleans prelude.core.equalities prelude.core.if_def

theorem excluded_middle : Π A : eta bool, eps (bor A (bnot A))
proof
admit
