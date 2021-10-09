// Usual prenex polymorphic equaltiy, with two arguments
require open
  personoj.lhol
  personoj.pvs_cert
  personoj.logical;

// We don't use prenex encoding to have implicit arguments.
symbol = {s: Set}: El s → El s → Prop;
notation = infix 2;
builtin "eq" ≔ =;

rule @= (@psub $t $p) $x $y ↪ @= $t (@fst $t $p $x) (@fst $t $p $y);

symbol ≠ {s: Set} (x y: El s) ≔ ¬ (x = y);
notation ≠ infix 2;

// Leibniz equality
rule Prf ($x = $y) ↪ Π p: El (_ ~> prop), Prf (p $x) → Prf (p $y);

// Some theorems for equality
opaque
symbol eq_refl {a: Set} (x: El a): Prf (x = x) ≔
begin
  refine λ _ x p (h: Prf (p x)), h;
end;
builtin "refl" ≔ eq_refl;

opaque
symbol eq_trans {a: Set} (x y z: El a) (_: Prf (x = y)) (_: Prf (y = z))
      : Prf (x = z) ≔
begin
  refine λ _ x y z (hxy: Prf (x = y)) (hyz: Prf (y = z)) p (px: Prf (p x)),
         hyz p (hxy p px);
end;
