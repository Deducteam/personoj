require open encodings.pvs_core prelude.core.booleans prelude.core.notequal

definition xor (A : eta bool) (B : eta bool) ≔ neq bool A B
