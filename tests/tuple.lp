require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.deptype
require open personoj.encodings.prenex
require open personoj.encodings.tuple

constant symbol nat: Set
constant symbol rat: Set


set verbose 3
constant symbol zn: η nat
constant symbol zq: η rat
definition tuple_zz ≔ tuple_cons _ zn _ (tuple_cons _ zq _ tuple_nil)
// assert tuple_car (type_cons nat _) (tuple_cons _ zn _ (tuple_cons _ zq _ tuple_nil)) ≡ zn
