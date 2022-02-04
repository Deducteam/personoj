// Curried equality
require open personoj.lhol personoj.pvs_cert personoj.logical;
require personoj.telescope as TL;
require personoj.eq as e;

symbol = [a: Set] (x y: El a): Prop ≔ e.= [a] (TL.double x y);

symbol != [a: Set] (x y: El a) ≔ e.!= [a] (TL.double x y);
