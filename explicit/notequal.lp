require open pvs_cert explicit.pts_bindings explicit.booleans
require open explicit.equalities

definition neq (T : Term Kind uType) (x y : Term Type T) ≔ bnot (eq T x y)
