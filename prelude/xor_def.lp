require open pvs_core prelude.booleans prelude.notequal

definition xor (A : eta bool) (B : eta bool) ≔ neq bool A B
