require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
require personoj.encodings.deferred as Deferred

constant symbol T: Set
symbol t: η T
symbol f: η (T ~> T)
constant symbol S: Set
symbol s: η S

set flag "print_implicits" on
definition qu_ko ≔ Deferred.app (Deferred.quote f) (Deferred.quote s)
compute Deferred.unquote qu_ko

definition qu_ok ≔ Deferred.app (Deferred.quote f) (Deferred.quote t)
compute Deferred.unquote qu_ok

compute Deferred.quote {T ~> T} (λx:η T, f x)
