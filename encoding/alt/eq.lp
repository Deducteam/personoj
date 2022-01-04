// Curried equality
require open personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical;
require personoj.eq as e;

symbol = {a: Set} (x y: El a): Prop ≔ e.= {a} (x ^^ y);

symbol != {a: Set} (x y: El a) ≔ e.!= {a} (x ^^ y);

