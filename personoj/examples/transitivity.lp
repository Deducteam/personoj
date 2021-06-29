// This file shows that coercions are transitive.
// We have approximately the following setup
//
//              numfield
//                / \
//               /   \
//              /     \
//        numfield*  real
//                     |
//                     |
//                   real*
//
// And we coerce an element (in [foox]) from [real*] to [numfield*].
require open personoj.lhol personoj.pvs_cert personoj.logical personoj.eqtup;
require open personoj.coercions;

require open personoj.extra.equality;

constant symbol numfield: Set;
constant symbol nz_numfield : El numfield → Prop;
symbol numfield* ≔ psub {numfield} nz_numfield;

constant symbol real_pred : El numfield → Prop;
symbol real ≔ psub {numfield} real_pred;

symbol nz_real : El real → Prop;
symbol real* ≔ psub {real} nz_real;

symbol foo : El numfield* → Prop;

debug +i;
symbol foox : Π x: El real*, Prf (foo x) 
begin admit; end;
