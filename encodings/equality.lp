// Usual prenex polymorphic equaltiy, with two arguments
require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.logical;

// We don't use prenex encoding to have implicit arguments.
symbol eq {s: Set}: El s → El s → Prop;
symbol = {s} ≔ @eq s;
set notation = infix 2;
set builtin "eq" ≔ eq;

rule @eq (@psub $t $p) $x $y ↪ @eq $t (@fst $t $p $x) (@fst $t $p $y);

symbol neq {s: Set} (x y: El s) ≔ ¬ (x = y);
symbol ≠ {s} ≔ @neq s;
set notation ≠ infix 2;

// Leibniz equality
rule Prf (eq $x $y) ↪ Π p: El (_ ~> prop), Prf (p $x) → Prf (p $y);

// Some theorems for equality
opaque
symbol eq_refl {a: Set} (x: El a): Prf (x = x) ≔
begin
  assume a x p h;
  apply h;
end;
set builtin "refl" ≔ eq_refl;

opaque
symbol eq_trans {a: Set} (x y z: El a) (_: Prf (x = y)) (_: Prf (y = z))
      : Prf (x = z) ≔
begin
  assume a x y z hxy hyz p px;
  refine hyz p (hxy p px);
end;
