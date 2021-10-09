require open personoj.lhol personoj.nat;

// Basic fixpoint with a measure
symbol fix (a: Set)(r: Set)(meas:El a → Nat)
           (f: Π(x:El a),
               (Π(y:El a)(π:Prf(lenat (meas y) (meas x))), El r) → El r):
  El a → El r;
rule fix $a $r $meas $f $x ↪
     $f $x
        (λ (y: El $a) (π:Prf(lenat ($meas y) ($meas $x))),
         fix $a $r $meas $f y);
