// Usual prenex polymorphic equaltiy, with two arguments
require open personoj.encodings.lhol
require open personoj.encodings.logical
require open personoj.encodings.prenex

// We don't use prenex encoding to have implicit arguments.
symbol eq {T: Set}: El T → El T → Prop
set infix left 2 "=" ≔ eq
set builtin "eq" ≔ eq

definition neq {s: Set} (x y: El s) ≔ ¬ (x = y)
set infix 2 "≠" ≔ neq

// Leibniz equality
rule Prf ($x = $y) ↪ Πp: El (_ ~> prop), Prf (p $x) → Prf (p $y)

// Some theorems for equality
theorem eq_refl {a: Set} (x: El a): Prf (x = x)
proof
  assume a x p h
  apply h
qed
set builtin "refl" ≔ eq_refl

theorem eq_trans {a: Set} (x y z: El a) (_: Prf (x = y)) (_: Prf (y = z))
      : Prf (x = z)
proof
  assume a x y z hxy hyz p px
  refine hyz p (hxy p px)
qed
