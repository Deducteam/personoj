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
  personoj.lhol personoj.pvs_cert personoj.logical personoj.eq;
require open personoj.coercions;
require personoj.telescope as TL;

constant symbol numfield: Set;
constant symbol nz_numfield : El numfield → Prop;
symbol numfield* ≔ psub [numfield] nz_numfield;

constant symbol real_pred : El numfield → Prop;
symbol real ≔ psub [numfield] real_pred;

symbol nz_real : El real → Prop;
symbol real* ≔ psub [real] nz_real;

// Symbols made to trigger coercions

symbol foo : El numfield* → Prop;
flag "print_implicits" on;
symbol foox : Π x: El real*, Prf (foo x)
begin /* TODO: don't rewrite protected symbols */ admitted;

// With a tuple
symbol bar: El (TL.code (TL.double! numfield numfield*)) → Prop;
symbol barx : Π x: El real, Π y: El real*, Prf (bar (TL.double x y))
begin /* TODO: don't rewrite protected symbols */ admitted;
