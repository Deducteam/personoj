require open encodings.pvs_core prelude.core.booleans prelude.core.notequal
prelude.core.equalities prelude.core.if_def

definition xor (A : η bool) (B : η bool) ≔ neq A B

theorem xor_def (A B: η bool) : ε (eq (xor A B) (if A (¬ B) B))
proof admit
