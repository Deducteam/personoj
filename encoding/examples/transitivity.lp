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
require open
  personoj.lhol personoj.pvs_cert personoj.tuple personoj.logical personoj.eq;
require open personoj.coercions;

constant symbol numfield: Set;
constant symbol nz_numfield : El numfield → Prop;
symbol numfield* ≔ psub [numfield] nz_numfield;

constant symbol real_pred : El numfield → Prop;
symbol real ≔ psub [numfield] real_pred;

symbol nz_real : El real → Prop;
symbol real* ≔ psub [real] nz_real;

// Symbols made to trigger coercions

symbol foo : El numfield* → Prop;
symbol foox : Π x: El real*, Prf (foo x)
begin admit; end;

// With a tuple
symbol bar: El (σ (numfield && numfield*)) → Prop;
symbol barx : Π x: El real, Π y: El real*, Prf (bar (x ^^ y))
begin admit; end;
